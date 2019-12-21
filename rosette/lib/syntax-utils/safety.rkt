#lang racket/base

(require racket/require
         (multi-in racket (match list dict set function contract sequence))
         (multi-in syntax (parse id-table stx))
         rosette/lib/util/utility/main
         (only-in "../optimization.rkt"
                  set-syntax-safe
                  safe?
                  set-safe?
                  clear-safe
                  current-progress?)
         "shared.rkt"
         "reporter.rkt")

(provide transform-taint
         symbolic-store
         (struct-out define-info))

(struct define-info (rhs path from) #:transparent)

(define symbolic-store (make-free-id-table))
(define current-module-info (make-parameter #f))

(define/contract (inherit-safety annotated xs) (syntax? list? . -> . syntax?)
  (define result (foldl (λ (e acc)
                          (cond
                            [(and (set-safe? e) (not (safe? e))) 'FAILED]
                            [(member acc '(FAILED UNKNOWN)) acc]
                            [(not (set-safe? e)) 'UNKNOWN]
                            [else acc])) 'SAFE xs))
  (cond
    [(eq? result 'FAILED) (set-syntax-safe annotated #f)]
    [(eq? result 'SAFE) (set-syntax-safe annotated #t)]
    [else annotated]))

(define (normalize-binding-info b-path b-mod path)
  (cond
    [(expanding-path? b-path) (cons (car path) b-mod)]
    [else (cons b-path b-mod)]))

(define mark-safe!
  (syntax-parser
    #:literal-sets (kernel-literals)
    [(define-values (name) _)
     (cond
       [(dict-has-key? symbolic-store #'name) #f]
       [else
        ((current-reporter) 'safety-analysis/define/safe-at-fixpoint #'name)
        (free-id-table-set!
         symbolic-store
         #'name
         (define-info
           #t ; NOTE(Oak): this should be filled in later automatically
           (current-module-info)
           'define))
        #t])]
    [(define-values names _)
     (for/or ([name (stx->list #'names)])
       (cond
         [(dict-has-key? symbolic-store name) #f]
         [else
          ((current-reporter) 'safety-analysis/define-values/safe-at-fixpoint name)
          (free-id-table-set!
                symbolic-store
                name
                (define-info
                  #t ; NOTE(Oak): this should be filled in later automatically
                  (current-module-info)
                  'define-values))
               #t]))]
    [_ #f]))

(define ((set-non-lexical-safe expr) binding-info)
  (or (for/or ([binding-info binding-info])
        (cond
          [(eq? 'kernel (cdr binding-info)) (set-syntax-safe expr #f)] ; TODO(Oak): check
          [else (match-define (list b-id b-path b-mod ...) binding-info)
                (for/or ([(name def-info) (in-free-id-table symbolic-store)])
                  (match-define (define-info rhs path from) def-info)
                  (cond
                    [(and (equal? path (normalize-binding-info b-path b-mod path))
                          (eq? (syntax->datum name) b-id))
                     (cond
                       [(eq? from 'define)
                        (set-syntax-safe expr rhs)]
                       [(eq? from 'define-values)
                        (set-syntax-safe expr rhs)])]
                    [else #f]))]))
      (cond
        ;; guaranteed that (second (first binding-info)) is well-defined
        ;; if this code is reached
        [(and (expanding-path? (second (first binding-info)))
              #;(set-member? (current-top-level-bindings) expr))
         expr]
        [else (set-syntax-safe expr #f)])))

#;(define current-top-level-bindings (make-parameter #f))

#;(define add-top-level-bindings!
  (syntax-parser
    #:literal-sets ([kernel-literals #:phase 0])
    [(define-values names _)
     (for ([name (stx->list #'names)])
       (current-top-level-bindings
        (free-id-set-add (current-top-level-bindings) name)))]
    [(begin . xs) (for-each add-top-level-bindings! (stx->list #'xs))]
    [_ (void)]))

(define (new-module-level stx current-module-info)
  (define source (syntax-source stx))
  (match (current-module-info)
    [(list path mod ...)
     (define name (syntax->datum stx))
     #;(printf "old-path: ~a, new-path: ~a, mods: ~a\n" path source `(,@mod ,name))
     ;; NOTE(Oak): it's unclear what path we should use. Use the old one for now
     `(,path ,@mod ,name)]
    [else (list source)]))

(define (transform-taint stx phase)

  (define (trans-expr stx)
    (define (trans-exprs stxs)
      (define tainted-stxs (stx-map trans-expr stxs))
      (inherit-safety (rebuild stx (stx-map cons stxs tainted-stxs)) tainted-stxs))
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      [(define-values names rhs)
       (define result (trans-exprs #'(rhs)))
       (syntax-parse result
         [(_ (name) tainted-rhs)
          (when (set-safe? #'tainted-rhs)
            (when (safe? #'tainted-rhs)
              ((current-reporter) 'safety-analysis/define/safe #'name))
            (free-id-table-set!
             symbolic-store
             #'name
             (define-info
               (and (safe? #'tainted-rhs) #'tainted-rhs)
               (current-module-info)
               'define)))]
         [(_ (names ...) tainted-rhs)
          (when (set-safe? #'tainted-rhs)
            (when (safe? #'tainted-rhs)
              ((current-reporter) 'safety-analysis/define-values/safe #'(names ...)))
            (for ([name (attribute names)])
              (free-id-table-set!
               symbolic-store
               name
               (define-info
                 (and (safe? #'tainted-rhs) #'tainted-rhs)
                 (current-module-info)
                 'define-values))))])
       result]
      [_:id
       (cond
         [(set-member? SAFE-BUILT-IN stx) (set-syntax-safe stx #t)]
         [(set-safe? stx) stx] ; safe by macro
         [(identifier-binding* stx) => (set-non-lexical-safe stx)]
         [else
          ;; NOTE(Oak): lexical, always safe
          ;; TODO: is this true? what about this?
          ;; (let ([f (λ () (set! x 1))]) (f))
          (set-syntax-safe stx #t)])]
      [(#%plain-lambda . form:lambda-body-form)
       (keep-lambda-properties stx (trans-exprs #'(form.body ...)))]
      [(case-lambda form:lambda-body-form ...)
       (keep-lambda-properties stx (trans-exprs #'({~@ form.body ...} ...)))]
      [(set! _ rhs)
       (define tainted-rhs (trans-expr #'rhs))
       ;; TODO: this should taint the id as unsafe?
       (set-syntax-safe (rebuild stx (list (cons #'rhs tainted-rhs))) #f)]
      [({~or* #%top #%variable-reference} . _) (set-syntax-safe stx #f)]
      [({~or* quote quote-syntax} . _) (set-syntax-safe stx #t)]
      [(_:let-values-form . form:let-values-body-form)
       ;; TODO: this should taint id?
       (trans-exprs #'(form.rhs ... form.body ...))]
      ;; TODO: is with-continuation-mark safe?
      [(#%plain-app . body) (trans-exprs #'body)]
      [(_:compound-form ~! . body) (trans-exprs #'body)]
      [_ (error 'transform "unrecognized expression form: ~.s" (syntax->datum stx))]))

  (define (trans-module-body bodies module-name)
    (parameterize (#;[current-top-level-bindings (immutable-free-id-set)])
      #;(for-each add-top-level-bindings! bodies)
      ((current-reporter) 'safety-analysis/start module-name)
      (define output
        (let loop ([this-queue (for/list ([b bodies] [i (in-naturals)]) (cons b i))]
                   [next-queue '()]
                   [progress? #f])
          (match this-queue
            ['() (match* (next-queue progress?)
                   [(_ #f)
                    ((current-reporter) 'safety-analysis/no-progress)
                    (if (for/or ([e next-queue]) (mark-safe! (car e)))
                        (loop (reverse next-queue) '() #f)
                        next-queue)]
                   [(_ #t)
                    ((current-reporter) 'safety-analysis/new-loop)
                    (loop (reverse next-queue) '() #f)])]
            [(cons (cons b i) rest-queue)
             (parameterize ([current-progress? #f])
               (define new-b (trans-top b 0))
               (define next-loop (curryr loop (or progress? (current-progress?))))
               (define item (cons new-b i))
               (next-loop rest-queue (cons item next-queue)))])))
      ((current-reporter) 'safety-analysis/end)
      (map car (sort output < #:key cdr))))

  (define trans-module
    (make-module-transformer
     #:custom
     (λ (body name)
       (parameterize ([current-module-info (new-module-level name current-module-info)])
         (let* ([bodys (syntax->list body)]
                [bodyl (trans-module-body bodys name)])
           (map cons bodys bodyl))))))

  (define (trans-top stx phase)
    (syntax-parse stx
      #:literal-sets ([kernel-literals #:phase phase])
      [(module _ _ _) (set-syntax-safe (trans-module this-syntax 0) #f)]
      [(module* _ init _) (set-syntax-safe (trans-module this-syntax (if (syntax-e #'init) 0 phase)) #f)]
      [({~or* #%provide #%declare begin-for-syntax #%require define-syntaxes} ~! . _)
       (set-syntax-safe this-syntax #f)]
      [_ (trans-expr this-syntax)]))

  (values ((compose trans-top transform-clear) stx phase) phase))

(define transform-clear
  (make-syntax-transformer
   (let ()
     (define (trans-expr stx)
       (define trans-exprs (make-exprs-transformer trans-expr stx))
       (clear-safe
        (syntax-parse stx
          #:literal-sets (kernel-literals)
          [(define-values _ rhs) (trans-exprs #'(rhs))]
          [_:id stx]
          [(#%plain-lambda . form:lambda-body-form)
           (keep-lambda-properties stx (trans-exprs #'(form.body ...)))]
          [(case-lambda form:lambda-body-form ...)
           (keep-lambda-properties stx (trans-exprs #'({~@ form.body ...} ...)))]
          [(set! _ rhs) (trans-exprs #'(rhs))]
          [(_:leaf-form ~! . _) stx]
          [(_:let-values-form . form:let-values-body-form)
           (trans-exprs #'(form.rhs ... form.body ...))]
          [(_:compound-form ~! . body) (trans-exprs #'body)]
          [_ (error 'transform "unrecognized expression form: ~.s" (syntax->datum stx))])))
     trans-expr)))

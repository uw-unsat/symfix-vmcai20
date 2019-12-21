#lang racket/base

(require racket/list
         racket/set
         racket/function
         racket/match
         syntax/id-table
         syntax/id-set
         syntax/stx
         syntax/parse
         (prefix-in rosette: rosette)
         (only-in rosette/base/form/control branch-and-merge)
         (only-in "../optimization.rkt" set-syntax-safe safe?)
         "../syntax-utils/safety.rkt"
         rosette/lib/util/utility/main

         "reporter.rkt"
         "shared.rkt")

(provide transform-deforest current-deforest reset-deforest)

(define current-deforest (make-parameter #f))
(define current-deforest-local (make-parameter #f))
(define current-expanding-ids (make-parameter (immutable-free-id-set)))

(define (reset-deforest)
  (current-deforest #f)
  (current-deforest-local)
  (current-expanding-ids (immutable-free-id-set)))

(define (ok-id? stx)
  (cond
    [(and (syntax? stx) (identifier? stx))
     (cond
       [(syntax-property stx 'symfix:foreign?) #f]
       [else #t])]
    [(syntax? stx) (ok-id? (syntax-e stx))]
    [(pair? stx) (and (ok-id? (car stx)) (ok-id? (cdr stx)))]
    [else #t]))

(define (package-syntax stx)
  (cond
    [(and (syntax? stx) (identifier? stx))
     (define binding-info (identifier-binding* stx))
     (cond
       [(syntax-property stx 'symfix:foreign?) stx]
       [(and binding-info
             (for/or ([x binding-info]) (and (list? x) (expanding-path? (second x)))))
        (syntax-property stx 'symfix:foreign? #t)]
       [else stx])]
    [(syntax? stx) (datum->syntax stx (package-syntax (syntax-e stx)) stx stx)]
    [(pair? stx) (cons (package-syntax (car stx)) (package-syntax (cdr stx)))]
    [else stx]))

(define ((try-replace-id expr) binding-info)
  (and=>
   (for/or ([binding-info binding-info])
     (cond
       [(eq? 'kernel (cdr binding-info)) #f]
       [else
        (match-define (list b-id b-path b-mod ...) binding-info)
        ;; Case 1: same or other module
        (free-id-table-ref
         symbolic-store
         expr
         ;; Case 2: other module
         (for/or ([(name def-info) (in-free-id-table symbolic-store)])
           (match-define (define-info rhs path from) def-info)
           (and (equal? path (rest binding-info))
                (eq? (syntax->datum name) (car binding-info))
                def-info)))]))
   => (λ (def-info)
        (match-define (define-info rhs path from) def-info)
        (and (eq? from 'define) (ok-id? rhs) rhs))))

;; Try to replace body with replacements
;; If the replacements have gone wrong, fallback to orig-stx
(define (try-replace body replacements orig-stx loop)
  (syntax-parse orig-stx
    #:literal-sets (kernel-literals)
    [(#%plain-app lam . args)
     #:when (and (safe? #'lam) (andmap safe? (stx->list #'args)))
     ((current-reporter) 'deforest/inline-app/succeed #'lam #'args)
     (define-values (result partitions)
       (parameterize ([current-replacements '()])
         (define result
           (rebuild body replacements
                    #:collect? #t
                    #:assoc (λ (v lst)
                              (cond
                                [(identifier? v)
                                 (assf (curry free-identifier=? v) lst)]
                                [else #f]))))
         (define partitions
           (group-by identity (current-replacements) free-identifier=?))
         (values result partitions)))
     (cond
       ;; TODO(Oak): for now, we say that if the function uses an argument
       ;; multiple times, then we will fall back to the unoptimized
       ;; version. This is to prevent multiple evaluation
       ;; There obviously is a better solution to this
       [(ormap (λ (part) (> (length part) 1)) partitions) orig-stx]
       [else
        ;; TODO NOW: need to mark this?
        (loop result)])]
    [(#%plain-app lam . args)
     ((current-reporter) 'deforest/inline-app/fail #'lam #'args)
     orig-stx]))

(define adjust-loc (make-adjust-loc 0))

(define (try-inline-app stx loop)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(#%plain-app (#%plain-lambda formals body) . args)
     (define e-formals (syntax-e #'formals))
     (define e-args (syntax-e #'args))
     (define len (length-improper e-formals))
     (cond
       [(>= (length e-args) len)
        (define-values (prefix-args suffix-args) (split-at e-args len))
        (define-values (prefix-formals suffix-formals) (split-at e-formals len))
        (cond
          [(and (empty? suffix-args) (empty? suffix-formals))
           (try-replace #'body (map cons e-formals e-args) stx loop)]
          [(empty? suffix-formals) (syntax-property stx 'skip-arity #t)]
          [else
           (try-replace
            #'body
            (cons
             (cons (if (symbol? suffix-formals) #'formals suffix-formals)
                   (with-syntax ([(SF ...) suffix-args])
                     (set-syntax-safe (adjust-loc #'(#%plain-app rosette:list SF ...)) #t)))
             (map cons prefix-formals prefix-args))
            stx
            loop)])]
       [else (syntax-property stx 'skip-arity #t)])]
    [(#%plain-app (case-lambda clauses rst ...) . args)
     (define stx (syntax/prop
                  this-syntax
                  #`(#%plain-app
                     #,(set-syntax-safe #'(#%plain-lambda . clauses)
                                        (safe? #'clauses))
                     . args)))
     (define marked (loop stx))
     (cond
       [(syntax-property marked 'skip-arity)
        (loop (syntax/prop this-syntax #'(#%plain-app (case-lambda rst ...) . args)))]
       ;; NOTE(Oak): this trick is cute, but it could make error message
       ;; less accurate. E.g.,
       ;;
       ;; ((case-lambda [(a b) 1]) 1)
       ;;
       ;; would result in
       ;;
       ;; arity mismatch;
       ;; the expected number of arguments does not match the given number
       ;; expected: 2
       ;; given: 1
       ;;
       ;; but with the optimization, it reports:
       ;;
       ;; arity mismatch;
       ;; the expected number of arguments does not match the given number
       ;; given: 1
       ;;
       ;; instead
       [else marked])]
    [(#%plain-app {~literal rosette:foldr}
                  maybe-f
                  maybe-base
                  (#%plain-app {~literal rosette:build} maybe-arg))
     (current-deforest-local #t)
     (let0 [v (loop (adjust-loc #'(#%plain-app maybe-arg maybe-f maybe-base)))]
       ((current-reporter) 'deforest/optimized stx v))]
    [(#%plain-app {~literal rosette:vector-ref}
                  (#%plain-app {~literal rosette:ref*} container acc-idx ...)
                  idx)
     ((current-reporter) 'deforest/optimized-ref stx)
     (current-deforest-local #t)
     (syntax/prop this-syntax
                  #`(#%plain-app rosette:ref* container acc-idx ...
                                 #,(adjust-loc #'(#%plain-app cons 'vector idx))))]
    [(#%plain-app {~literal rosette:list-ref}
                  (#%plain-app {~literal rosette:ref*} container acc-idx ...)
                  idx)
     ((current-reporter) 'deforest/optimized-ref stx)
     (current-deforest-local #t)
     (syntax/prop this-syntax
                  #`(#%plain-app rosette:ref* container acc-idx ...
                                 #,(adjust-loc #'(#%plain-app cons 'list idx))))]
    [(#%plain-app {~literal rosette:vector-ref}
                  container
                  idx)
     (syntax/prop this-syntax
                  #`(#%plain-app rosette:ref* container
                                 #,(adjust-loc #'(#%plain-app cons 'vector idx))))]
    [(#%plain-app {~literal rosette:list-ref}
                  container
                  idx)
     (syntax/prop this-syntax
                  #`(#%plain-app rosette:ref* container
                                 #,(adjust-loc #'(#%plain-app cons 'list idx))))]
    [_ stx]))

(define (make-special id) (syntax-property id 'symfix:synthetic? #t))

(define (make-safe stx)
  (cond
    [(and (syntax? stx) (identifier? stx)) (set-syntax-safe (adjust-loc stx) #t)]
    [(syntax? stx)
     (set-syntax-safe
      (adjust-loc (datum->syntax stx (make-safe (syntax-e stx)) stx stx))
      #t)]
    [(pair? stx) (cons (make-safe (car stx)) (make-safe (cdr stx)))]
    [else stx]))

(define (copy-syntax stx)
  (cond
    [(and (syntax? stx) (identifier? stx)) (adjust-loc stx)]
    [(syntax? stx)
     (adjust-loc (datum->syntax stx (copy-syntax (syntax-e stx)) stx stx))]
    [(pair? stx) (cons (copy-syntax (car stx)) (copy-syntax (cdr stx)))]
    [else stx]))

(define transform-deforest/core
  (make-syntax-transformer
   (let ()
     (define (trans-expr stx)
       (define (trans-exprs stxs) (rebuild stx (stx-map (λ (e) (cons e (trans-expr e))) stxs)))
       (syntax-parse stx
         #:literal-sets (kernel-literals)
         ;; NOTE(Oak): synthetic syntax is protected and freezed, and not further expanded.
         [_
          #:when (syntax-property stx 'symfix:synthetic?)
          stx]

         [{~literal rosette:map}
          (make-safe
           #`(case-lambda
               [(f xs)
                (#%plain-app
                 rosette:build
                 (#%plain-lambda
                  (link mt)
                  (#%plain-app
                   rosette:foldr
                   (#%plain-lambda (e acc)
                                   (#%plain-app
                                    link
                                    (#%plain-app f e)
                                    acc))
                   mt
                   xs)))]
               [(f xs . rest-id)
                (#%plain-app rosette:apply
                             #,(make-special #'rosette:map) f xs rest-id)]))]
         [{~literal rosette:filter}
          (make-safe
           #'(#%plain-lambda
              (f xs)
              (#%plain-app
               rosette:build
               (#%plain-lambda
                (link mt)
                (#%plain-app
                 rosette:foldr
                 (#%plain-lambda
                  (e acc)
                  (#%plain-app
                   branch-and-merge
                   (#%plain-app f e)
                   (#%plain-lambda () (#%plain-app link e acc))
                   (#%plain-lambda () acc)))
                 mt
                 xs)))))]
         [{~literal rosette:length}
          (make-safe
           #'(#%plain-lambda
              (xs)
              (#%plain-app
               rosette:foldr
               (#%plain-lambda
                (e acc)
                (#%plain-app rosette:add1 acc))
               (quote 0)
               xs)))]
         [{~literal rosette:filter-not}
          (trans-expr (make-safe
                       #'(#%plain-lambda
                          (f xs)
                          (#%plain-app
                           rosette:filter
                           (#%plain-lambda
                            (x)
                            (#%plain-app rosette:not (#%plain-app f x)))
                           xs))))]

         [(define-values _ rhs)
          (define marked (trans-exprs #'(rhs)))
          (syntax-parse marked
            [(_ (name) marked-rhs)
             (match-define (define-info _ module-info 'define)
               (free-id-table-ref symbolic-store #'name))
             (free-id-table-set!
              symbolic-store
              #'name
              (define-info
                (and (safe? #'marked-rhs) (package-syntax #'marked-rhs))
                module-info
                'define))]
            [_ (void)])
          marked]

         [_:id
          (define rhs (cond
                        [(set-member? (current-expanding-ids) stx) #f]
                        [(identifier-binding* stx) => (try-replace-id stx)]
                        [else #f]))
          (cond
            ;; NOTE(Oak): ideally, there should be no need to make a recursive call
            ;; here because we should have an invariant that the callee should be
            ;; completely inlined already. Unfortunately, this is not true
            ;; in ONE case:
            ;;
            ;; (define (foo) (bar))
            ;; (define (bar) 1)
            ;; (foo)
            ;;
            ;; because bar appears after foo, bar is not yet inlined.
            [rhs ((current-reporter) 'deforest/inline-id stx rhs)
                 (parameterize ([current-expanding-ids
                                 (free-id-set-add (current-expanding-ids) stx)])
                   (copy-syntax (trans-expr rhs)))]
            [else (make-special stx)])]

         [(#%plain-lambda . form:lambda-body-form)
          (keep-lambda-properties stx (trans-exprs #'(form.body ...)))]
         [(case-lambda form:lambda-body-form ...)
          (keep-lambda-properties stx (trans-exprs #'({~@ form.body ...} ...)))]
         [(set! _ rhs) (trans-exprs #'(rhs))]

         [(#%plain-app . body) (try-inline-app (trans-exprs #'body) trans-expr)]

         [(_:leaf-form ~! . _) stx]
         [(_:let-values-form . form:let-values-body-form)
          (trans-exprs #'(form.rhs ... form.body ...))]
         [(_:compound-form ~! . body) (trans-exprs #'body)]
         [_ (error 'transform "unrecognized expression form: ~.s" (syntax->datum stx))]))
     trans-expr)))

(define (transform-deforest stx phase)
  (parameterize ([current-deforest-local #f])
    (define new-stx ((compose (proj 0) transform-deforest/core transform-taint) stx phase))
    (current-deforest (or (current-deforest) (current-deforest-local)))
    (values (if (current-deforest-local) new-stx stx) phase)))

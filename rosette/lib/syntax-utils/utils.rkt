#lang racket/base

(require racket/function
         racket/sequence
         racket/string
         racket/match
         racket/syntax
         racket/format
         syntax/parse
         syntax/stx
         rosette/lib/util/utility/main)

(provide id=?
         rosette-module?
         transform-normalize
         make-rosette-compiler
         no-set!?)

(define (id=? id name)
  (and (identifier? id) (equal? (syntax-e id) name)))

(define (rosette-module? stx)
  (syntax-case stx ()
    [(mod id lang (mod-begin forms ...))
     (and (id=? #'mod 'module)
          (or (id=? #'lang 'rosette)
              (id=? #'lang 'rosette/safe))
          (id=? #'mod-begin '#%module-begin))
     #'id]
    [_ #f]))

(define (no-set!? target stx)
  (define set!? #f)
  (define (trans-expr stx)
    (define (trans-exprs stxs) (for ([e (in-syntax stxs)]) (trans-expr e)))
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      [_:id #f]
      [(_:leaf-form ~! . _) #f]
      [(define-values _ rhs) (trans-expr #'rhs)]
      [(#%plain-lambda . form:lambda-body-form) (trans-exprs #'(form.body ...))]
      [(case-lambda form:lambda-body-form ...) (trans-exprs #'({~@ form.body ...} ...))]
      [(set! x _)
       #:when (free-identifier=? #'x target)
       (set! set!? #t)]
      [(set! _ rhs) (trans-exprs #'(rhs))]
      [(_:let-values-form . form:let-values-body-form)
       (trans-exprs #'(form.rhs ... form.body ...))]
      [(_:compound-form ~! . body) (trans-exprs #'body)]
      [_ (error 'transform "unrecognized expression form: ~.s" (syntax->datum stx))]))
  (trans-expr stx)
  (not set!?))

(define (split-body xs)
  (syntax-parse xs
    [(x) #'x]
    [(x . xs) (syntax/prop #'x #`(let-values () x #,(split-body #'xs)))]))

(define transform-normalize
  (make-syntax-transformer
   (let ()
     (define (trans-expr stx)
       (define trans-exprs (make-exprs-transformer trans-expr stx))
       (syntax-parse stx
         #:literal-sets (kernel-literals)
         [(#%plain-app f a . args)
          #:with {~and result (_ . args-new)} (trans-exprs #'(f a . args))
          (define-values (result-args done performed?)
            (for/fold ([result-args '()] [done identity] [performed? #f]) ([arg (in-syntax #'args-new)])
              (syntax-parse arg
                #:literal-sets (kernel-literals)
                [{~or* _:id ({~or* quote quote-syntax} . _)} (values (cons arg result-args) done performed?)]
                [_ (define new-id (syntax/prop this-syntax (generate-temporary 'gensym-for-app)))
                   (values (cons new-id result-args)
                           (λ (stx) (done (syntax/prop this-syntax #`(let-values ([(#,new-id) #,arg]) #,stx))))
                           #t)])))
          (if performed?
              (done (syntax/prop this-syntax #`(#%plain-app . #,(reverse result-args))))
              #'result)]
         [(define-values _ rhs) (trans-exprs #'(rhs))]
         [_:id stx]
         [(#%plain-lambda . form:lambda-body-form)
          #:with (_ . new-form:lambda-body-form) (trans-exprs #'(form.body ...))
          #:with splitted (split-body #'(new-form.body ...))
          (keep-lambda-properties
           this-syntax
           (syntax/prop this-syntax #'(#%plain-lambda new-form.fmls splitted)))]
         [(case-lambda form:lambda-body-form ...)
          #:with (_ new-form:lambda-body-form ...) (trans-exprs #'({~@ form.body ...} ...))
          #:with (splitted ...) (stx-map split-body #'((new-form.body ...) ...))
          (keep-lambda-properties
           this-syntax
           (syntax/prop this-syntax #'(case-lambda [new-form.fmls splitted] ...)))]
         [(set! _ rhs) (trans-exprs #'(rhs))]
         [(_:leaf-form ~! . _) stx]
         [(let-values () . body)
          #:with (_ () . new-body) (trans-exprs #'body)
          (split-body #'new-body)]
         [(let-values . form:let-values-body-form)
          #:when (> (length (attribute form.rhs)) 1)
          #:with (_ (bindings ... last-binding) . body-new) (trans-exprs #'(form.rhs ... form.body ...))
          #:with splitted (split-body #'body-new)
          (define done
            (for/fold ([done identity]) ([binding (attribute bindings)])
              (λ (stx) (done (syntax/prop this-syntax #`(let-values (#,binding) #,stx))))))
          (done (syntax/prop this-syntax #'(let-values (last-binding) splitted)))]
         [(typ:let-values-form . form:let-values-body-form)
          #:with (_ bindings . body-new) (trans-exprs #'(form.rhs ... form.body ...))
          #:with splitted (split-body #'body-new)
          (syntax/prop this-syntax #'(typ bindings splitted))]
         [(begin body) (trans-expr #'body)]
         [(_:compound-form ~! . body) (trans-exprs #'body)]
         [_ (error 'transform "unrecognized expression form: ~.s"
                   (syntax->datum stx))]))
     trans-expr)))

(define (transform-count stx)
  (syntax-parse stx
    [(a . b) (+ (transform-count #'a) (transform-count #'b))]
    [() 0]
    [_ 1]))

(define (make-rosette-compiler transformer
                               #:instant? [instant? #f]
                               #:base [base-compiler (current-compile)]
                               #:size-limit [size-limit #f]
                               #:dump [dump #f]
                               #:before [before identity])
  (define first-processed? #f)
  (make-compiler
   (λ (stx)
     (define first-processed-now? first-processed?)
     (set! first-processed? #t)
     (define phase (namespace-base-phase))
     (define output-stx
       (cond
         [(rosette-module? stx)
          (printf "COMPILING ~v\n" (syntax-source stx))
          (define size (transform-count stx))
          (cond
            [(and size-limit (> size size-limit))
             (printf "ELIDING DUE TO SIZE LIMIT (~a)\n" size)
             (expand-syntax stx)]
            [else (define out ((compose (proj 0) transformer)
                               (expand-syntax (before stx)) phase))
                  (when dump (dump base-compiler out))
                  out])]
         [else (expand-syntax stx)]))
     (cond
       [first-processed-now? output-stx]
       [instant? (syntax-shift-phase-level
                  #'(module a racket/base (#%plain-module-begin))
                  phase)]
       [else output-stx]))
   #:base base-compiler))

#lang racket/base

(provide lambda-body-form
         let-values-body-form
         let-values-form
         leaf-form
         compound-form
         transform-disarm
         make-top-transformer
         make-exprs-transformer
         make-module-transformer
         make-syntax-transformer
         syntax-exists?)

(require racket/list
         racket/contract
         racket/function
         syntax/parse
         syntax/parse/lib/function-header
         syntax/stx
         loop
         "../debug.rkt"
         "./object.rkt")

(define code-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))

(define-syntax-class let-values-body-form
  (pattern [([(ids:id ...) rhs:expr] ...) body:expr ...+]))

(define-syntax-class lambda-body-form
  (pattern [fmls:formals body:expr ...+]))

(define-syntax-class leaf-form
  #:literal-sets (kernel-literals)
  (pattern {~or* quote quote-syntax #%top #%variable-reference}))

(define-syntax-class compound-form
  #:literal-sets (kernel-literals)
  (pattern {~or* if begin begin0 #%plain-app with-continuation-mark}))

(define-syntax-class let-values-form
  #:literal-sets (kernel-literals)
  (pattern {~or* let-values letrec-values}))

(define (transform-disarm stx phase)
  (define (trans-expr stx)
    (let ([stx (syntax-disarm stx code-insp)])
      (define trans-exprs (make-exprs-transformer trans-expr stx))
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

  (define trans-module (make-module-transformer #:trans-top-thunk (thunk trans-top)))

  (define trans-top
    (make-top-transformer trans-expr trans-module
                          #:pre-trans (curryr syntax-disarm code-insp)))

  (values (trans-top stx phase) phase))

(define ((make-exprs-transformer trans-expr stx) stxs)
  (rebuild stx (stx-map (λ (e) (cons e (trans-expr e))) stxs)))

(define (make-module-transformer #:trans-top-thunk [trans-top-thunk #f] #:custom [custom #f])
  (cond
    [trans-top-thunk
     (make-module-transformer
      #:custom (λ (body name) (stx-map (λ (e) (cons e ((trans-top-thunk) e 0))) body)))]
    [custom
     (λ (stx phase)
       (define shifted (syntax-shift-phase-level stx (- phase)))
       (syntax-parse shifted
         [(_ name _ (_ . body))
          (parameterize ([current-module-path (syntax-source #'name)])
            (syntax-shift-phase-level
             (rebuild shifted (custom #'body #'name))
             phase))]))]
    [else (error 'make-module-transformer)]))

(define ((make-top-transformer trans-expr trans-module #:pre-trans [pre-trans identity]) stx phase)
  (syntax-parse (pre-trans stx)
    #:literal-sets ([kernel-literals #:phase phase])
    [(module _ _ _) (trans-module this-syntax 0)]
    [(module* _ init _) (trans-module this-syntax (if (syntax-e #'init) 0 phase))]
    [({~or* #%provide #%declare begin-for-syntax #%require define-syntaxes} ~! . _) this-syntax]
    [_ (trans-expr this-syntax)]))

(define ((make-syntax-transformer trans-expr) stx phase)
  (define trans-module (make-module-transformer #:trans-top-thunk (thunk trans-top)))
  (define trans-top (make-top-transformer trans-expr trans-module))
  (values (trans-top stx phase) phase))

(define (syntax-exists? fragment)
  (make-syntax-transformer
   (let ()
     (define (trans-expr stx)
       (define (trans-exprs stxs)
         (rebuild stx (stx-map (λ (e) (cons e (trans-expr e))) stxs)))
       (syntax-parse stx
         #:literal-sets (kernel-literals)
         [_
          #:when (eq? stx fragment)
          (debug: fragment #:msg "found syntax!" #:fg 'cyan)
          stx]
         [(define-values _ rhs) (trans-exprs #'(rhs))]
         [_:id stx]
         [(#%plain-lambda . form:lambda-body-form)
          (trans-exprs #'(form.body ...))]
         [(case-lambda form:lambda-body-form ...)
          (trans-exprs #'({~@ form.body ...} ...))]
         [(set! _ rhs) (trans-exprs #'(rhs))]
         [(_:leaf-form ~! . _) stx]
         [(_:let-values-form . form:let-values-body-form)
          (trans-exprs #'(form.rhs ... form.body ...))]
         [(_:compound-form ~! . body) (trans-exprs #'body)]
         [_ (error 'transform "unrecognized expression form: ~.s" (syntax->datum stx))]))
     trans-expr)))

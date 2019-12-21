#lang racket/base

(require syntax/stx
         syntax/parse
         syntax/id-set
         racket/match
         racket/syntax
         racket/function
         (only-in rosette simplify-condition)
         rosette/lib/util/utility/main
         "structs.rkt"
         (only-in "../../../syntax-utils/utils.rkt" id=?)
         "../../optimize-reporter.rkt")

(provide transform-serval-hack)

(define (transform-serval-hack target-expr)
  (define (make-fix stx) #`(simplify-condition #,stx))
  (make-syntax-transformer
    (let ()
      (define (trans-expr stx strict?)
        (define (trans-exprs stxs)
          (rebuild stx (stx-map (λ (e) (cons e (trans-expr e #f))) stxs)))
        (syntax-parse stx
          #:literal-sets (kernel-literals)
          [_
           #:when (and (not strict?) (eq? stx target-expr))
           (make-fix (trans-expr stx #t))]
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
          [_ (error 'transform "unrecognized expression form: ~.s"
                    (syntax->datum stx))]))
      (curryr trans-expr #f))))

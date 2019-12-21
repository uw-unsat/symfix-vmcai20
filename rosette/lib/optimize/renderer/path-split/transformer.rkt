#lang racket/base

(require syntax/stx
         syntax/parse
         syntax/id-set
         racket/match
         racket/syntax
         racket/function
         (only-in rosette split-all)
         rosette/lib/util/utility/main
         "structs.rkt"
         (only-in "../../../syntax-utils/utils.rkt" id=?)
         "../../optimize-reporter.rkt")

(provide transform-path-split)

(define (transform-path-split forall-expr target-expr type)
  (define (make-split-all stx)
    (match type
      [(or "union" "struct") #`(split-all (#,target-expr) #,stx)]))
  (make-syntax-transformer
    (let ()
      (define (trans-expr stx strict?)
        (define (trans-exprs stxs)
          (rebuild stx (stx-map (λ (e) (cons e (trans-expr e #f))) stxs)))
        (syntax-parse stx
          #:literal-sets (kernel-literals)
          [_
           #:when (and (not strict?) (eq? stx forall-expr))
           (make-split-all (trans-expr stx #t))]
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



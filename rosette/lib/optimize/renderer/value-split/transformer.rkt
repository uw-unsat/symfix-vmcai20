#lang racket/base

(require syntax/stx
         syntax/parse
         (only-in rosette/base/struct/struct-type
                  [@make-struct-type rosette:make-struct-type])
         rosette/lib/util/utility/main)

(provide transform-value-split)

(define (transform-value-split name)
  (make-syntax-transformer
   (let ()
     (define (trans-expr stx)
       (define trans-exprs (make-exprs-transformer trans-expr stx))
       (syntax-parse stx
           #:literal-sets (kernel-literals)

           [(#%plain-app {~literal rosette:make-struct-type} (quote label) body ...)
            #:when (and (equal? (syntax->datum #'label) name)
                        (= 10 (length (attribute body))))
            #'(#%plain-app rosette:make-struct-type (quote label) body ... (quote #t))]

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
     trans-expr)))

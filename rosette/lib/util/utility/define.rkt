#lang racket/base

(provide define-multiple define/who)
(require syntax/parse/define
         racket/stxparam
         (for-syntax racket/base
                     racket/syntax))

(begin-for-syntax
  (define-splicing-syntax-class trans-class
    (pattern {~seq #:trans {~and clauses (f ...)}}))
  (define-splicing-syntax-class prefix-class
    (pattern {~seq #:prefix id:id}))
  (define-syntax-class (new-head pre)
    (pattern id:id #:with reassembled (format-id #'id "~a~a" (or pre "") #'id))
    (pattern [{~var head (new-head pre)} rst ...]
             #:with reassembled #'[head.reassembled rst ...])))

(define-simple-macro
  (define-multiple
    {~alt {~optional trans:trans-class}
          {~optional pre:prefix-class}} ...
    [{~var head (new-head (attribute pre.id))} body ...] ...)
  (define-multiple/core {~? trans.clauses []} [head.reassembled body ...] ...))

(define-syntax-parser define-multiple/core
  [(_ () [head body ...] ...) #'(begin (define head body ...) ...)]
  [(_ (hd tl ...) [head body ...] ...)
   #'(define-multiple/core [tl ...] [head (hd body ...)] ...)])

(define-syntax-parameter who (λ (stx) (raise-syntax-error (syntax-e stx) "not allowed")))

(define-simple-macro (define/who (id:id . args) . body)
  (define id
    (let ([the-id 'id])
      (syntax-parameterize ([who (make-rename-transformer #'the-id)])
        (λ args . body)))))

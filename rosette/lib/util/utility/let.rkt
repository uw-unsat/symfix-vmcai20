#lang racket/base

(provide (all-defined-out))
(require syntax/parse/define (for-syntax racket/base))

(define-simple-macro (let0 [x v] . body)
  (let ([x v])
    (begin . body)
    x))

(define-simple-macro (let/effect . xs) (let . xs))

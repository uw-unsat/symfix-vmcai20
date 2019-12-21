#lang racket/base

(provide (all-defined-out))
(require syntax/parse/define)

(define-simple-macro (define-macro (macro-id . pattern) . body)
  (define-syntax macro-id
    (syntax-parser
      [(_ . pattern) . body])))

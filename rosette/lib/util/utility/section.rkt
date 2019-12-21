#lang racket/base

(provide (all-defined-out))
(require syntax/parse/define)

(define-simple-macro (section desc:str body:expr ...)
  (begin body ...))

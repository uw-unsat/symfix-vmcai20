#lang racket/base

(provide (all-defined-out))
(require syntax/parse/define
         syntax/location
         (for-syntax racket/base))

(define-simple-macro (module/path name . rst)
  (begin
    (module name . rst)
    (define name (quote-module-path name))))

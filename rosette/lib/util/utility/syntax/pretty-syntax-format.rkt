#lang racket/base

(provide (all-defined-out))
(require racket/format
         racket/pretty)

(define (pretty-syntax-format x)
  (format "<pretty-syntax ~a ~a:~a: ~a>"
          (syntax-source x)
          (syntax-line x)
          (syntax-column x)
          (pretty-format (syntax->datum x))))

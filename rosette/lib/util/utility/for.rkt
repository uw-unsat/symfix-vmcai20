#lang racket/base

(provide (all-defined-out))
(require syntax/parse/define (for-syntax racket/base))

(define-simple-macro (for/hash! clauses b-b ... last-body)
  (let ([h (make-hash)])
    (for clauses
      b-b ...
      (let-values ([(k v) last-body])
        (hash-set! h k v)))
    h))

(define-simple-macro (for/filter-list whatever b-b ...)
  (filter values (for/list whatever b-b ...)))

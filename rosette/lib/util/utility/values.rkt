#lang racket/base

(provide proj)

(define (proj n) (λ xs (list-ref xs n)))

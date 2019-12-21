#lang racket/base

(provide (all-defined-out))
(require syntax/parse/define
         racket/function
         (for-syntax racket/base))

(define-simple-macro (change! x:id f:expr) (set! x (f x)))

(define (make-list-accum)
  (let ([xs '()] [absence (gensym)])
    (λ ([x absence] #:reset [reset #f])
      (when reset (set! xs '()))
      (if (eq? absence x)
          xs
          (change! xs (curry cons x))))))

(define (make-counter)
  (define cnt 0)
  (thunk (begin0 cnt (change! cnt add1))))

(module+ test
  (require rackunit)
  (define accum! (make-list-accum))
  (check-equal? (accum!) '())
  (accum! 1)
  (check-equal? (accum!) '(1))
  (accum! 2)
  (check-equal? (accum!) '(2 1))
  (accum! #:reset #t)
  (check-equal? (accum!) '())
  (accum! 1)
  (check-equal? (accum!) '(1)))

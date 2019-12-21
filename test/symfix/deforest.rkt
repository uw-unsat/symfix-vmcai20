#lang rosette

(define (sum xs)
  (foldr + 0 (filter positive? xs)))

(define-symbolic xs integer? [100])
(for ([x xs] [i (in-naturals)])
  (assert (= i x)))

(time (verify (assert (= (* 99 100 1/2) (sum xs)))))
(time (verify (assert (= 0 (sum xs)))))

#lang racket/base

(provide make-memoized)
(require racket/match
         racket/function
         (only-in "../core/bool.rkt" pc @assert with-asserts* @=>)
         (only-in "../form/define.rkt" define-symbolic*)
         (only-in "../../query/eval.rkt" evaluate)
         (only-in "../core/reflect.rkt" type-of term?)
         (only-in "../../solver/solution.rkt" sat))

(define cached (make-hasheq))

(define (compute-fresh f typ)
  (parameterize ([pc #t])
    (define-symbolic* x typ)
    (define-values (result asserted) (with-asserts* (f x)))
    (list x result asserted)))

(define (make-memoized f)
  (λ (input)
    (cond
      [(term? input)
       (match-define (list fresh result asserted)
         (hash-ref! cached f (thunk (compute-fresh f (type-of input)))))
       (define out (evaluate result (sat (hash fresh input))))
       (for ([asserted asserted])
         (@assert (@=> (pc) (evaluate asserted (sat (hash fresh input))))))
       out]
      [else (f input)])))

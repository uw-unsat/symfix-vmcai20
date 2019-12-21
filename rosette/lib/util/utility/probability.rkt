#lang racket/base

(provide (all-defined-out))
(require racket/function
         racket/list
         "./mutation.rkt"
         "./debug.rkt"
         "./double.rkt")

(define (pick-in-list xs
                      #:treshold [treshold 0.9]
                      #:measure [measure (const #f)])
  (define candidates (if (procedure? measure)
                         (for/list ([x xs]) (double (measure x) x))
                         (for/list ([x xs] [y measure]) (double y x))))
  (define-values (strict-candidates non-strict-candidates) (partition fst candidates))
  (define candidate
    (cond
      [(or (empty? non-strict-candidates)
           (and (not (empty? strict-candidates)) (< (random) treshold)))
       ;; we are picking strict candidates
       (define pick (random (apply + (map fst strict-candidates))))
       (findf (λ (candidate)
                (begin0 (< pick (fst candidate))
                  (change! pick (curryr - (fst candidate))))) strict-candidates)]
      [else (list-ref non-strict-candidates (random (length non-strict-candidates)))]))
  (snd candidate))

(define (pick-in-list/inverted xs
                               #:treshold [treshold 0.9]
                               #:measure [measure (const #f)])
  (define candidates (if (procedure? measure)
                         (for/list ([x xs]) (double (measure x) x))
                         (for/list ([x xs] [y measure]) (double y x))))
  (define max-val (add1 (apply max 0 (filter-map fst candidates))))
  (define new-candidates (for/list ([x candidates])
                           (define m (fst x))
                           (double (and m (- max-val m)) (snd x))))
  (snd (pick-in-list new-candidates #:treshold treshold #:measure fst)))

(define (metroprolis-hastlings-accept? cost new-cost)
  (min 1 (exp (* 2 (- cost new-cost)))))

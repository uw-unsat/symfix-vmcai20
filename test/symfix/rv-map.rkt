#lang rosette

(define (reverse-map fn lst)
  (let loop ([lst lst] [acc '()])
    (cond
      [(empty? lst) acc]
      [else (loop (rest lst) (cons (fn (first lst)) acc))])))

(define (check rm fn N)
  (protect
   (define-symbolic* n integer?)
   (define-symbolic* xs integer? [N])
   (define lst (take xs n))
   (verify (assert (equal? (rm fn lst) (reverse (map fn lst)))))))

(time (check reverse-map add1 50))
(hash-count (term-cache))

#lang racket

(provide ref*)
(require "./vector.rkt" "./list.rkt"
         (only-in "../core/real.rkt" @<= @<)
         (only-in "../core/safe.rkt" assert)
         (only-in "../core/forall.rkt" for/all))

(define (lookup type)
  (case type
    [(vector) (values @vector-length @vector? @vector-ref)]
    [(list) (values @length @list? @list-ref)]))

(struct double (fst snd) #:transparent)

(define (ref*/naive container xs)
  (let loop ([container container] [xs xs])
    (match xs
      ['() container]
      [(cons (cons type x) xs)
       (match-define-values (_ _ accessor) (lookup type))
       (loop (accessor container x) xs)])))

(define (ref* container . xs)
  (define result
    (let/ec bail-out
      (let loop ([container container] [xs xs] [acc '()])
        (match xs
          ['() (double '() (list (reverse acc)))]
          [(cons (cons type x) xs)
           (define-values (len container? accessor) (lookup type))
           (cond
             ;; uniform structure, concrete number
             [(and (number? x) (container? container) (number? (len container)) )
              (match-define (double arities result)
                (loop (accessor container x) xs (cons (cons type x) acc)))
              (double (cons (cons type (len container)) arities) result)]

             ;; uniform structure, symbolic number
             [(and (container? container) (number? (len container)))
              (define results
                (for/list ([x (len container)])
                  (loop (accessor container x) xs (cons (cons type x) acc))))

              (match (remove-duplicates (map double-fst results))
                [(list arities) (double (cons (cons type (len container)) arities)
                                        (append* (map double-snd results)))]
                [_ (bail-out #f)])]

             ;; non-uniform structure, or non-structure
             [else (bail-out #f)])]))))

  (match result
    [(double arities keys)
     (define keys* (remove-duplicates keys))
     (for ([x xs] [arity arities])
       (define real-arity (cdr arity))
       (define real-x (cdr x))
       (assert (@<= 0 real-x))
       (assert (@< real-x real-arity)))
     (for/all ([xs xs keys*]) (ref*/naive container xs))]
    [#f (ref*/naive container xs)]))

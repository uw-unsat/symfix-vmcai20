#lang racket/base

(require syntax/parse/define
         racket/match
         rosette/lib/util/utility/main
         "./polymorphic.rkt"
         "./term.rkt"
         "./union.rkt"
         "./type.rkt"
         "./forall.rkt"
         "./bool.rkt"
         "./equality.rkt"
         "./bitvector.rkt"
         "../struct/struct-type.rkt"
         (for-syntax racket/base))

(provide split-all
         split-all*
         simplify-condition
         get-mutator+field
         protect)

(define-simple-macro (protect xs ...)
  (begin xs ...))

(define (split-all-symfix* val proc)
  (for/all ([x val #:exhaustive]) (proc x)))

(define (get-mutator+field val #:pure? [pure? #f])
  (and (or (not pure?) (not (union? val)))
       (typed? val)
       (@struct-type? (get-type val))
       (let* ([st-type (get-type val)]
              [immutables (@struct-type-immutables st-type)]
              [num-fields (@struct-type-fields st-type)])
         (for/or ([i (in-range num-fields)])
           (define arg ((@make-struct-field-accessor val i 'dummy) val))
           (match arg
             [(or (? union?) (expression (or (== ite) (== ite*)) _ ...))
              (and (not (member i immutables))
                   (list (@make-struct-field-mutator val i 'dummy) arg))]
             [_ #f])))))

;; we try to be context sensitive here, though we really should split this
;; to two constructs as they are not mutually exclusive
(define (split-all-symfix val proc)
  (match (get-mutator+field val #:pure? #t)
    [(list mutator val-fld)
     (for/all ([x val-fld #:exhaustive])
       (begin (mutator val x)
              (proc val)))]
    [_ (for/all ([x val #:exhaustive]) (proc x))]))

(define-syntax-parser split-all
  [(_ (v:id) expr)
   (syntax/loc this-syntax (let ([tmp (lambda (v) expr)])
                             (split-all-symfix v tmp)))])

(define-syntax-parser split-all*
  [(_ (v:id val) expr)
   (syntax/loc this-syntax (let ([tmp (lambda (v) expr)])
                             (split-all-symfix* val tmp)))])

(define (simplify-condition expr)
        (match expr
          [(expression (== @!)
              (expression (== @bveq)
                (expression (== @bvadd) C1 x)
                (expression (== @bvadd) C2 y)))
          #:when (@&& (@! (term? C1))
                      (@! (term? C2))
                      (@! (@bveq C1 C2)))
          (define newexpr (@|| (@bveq x y) (@! (@bveq (@bvadd C1 x) (@bvadd C2 y)))))
          (@assert (@equal? newexpr expr))
          newexpr]
          [_ expr]))

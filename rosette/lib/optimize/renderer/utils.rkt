#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/struct
         racket/generator
         racket/contract
         racket/list
         racket/match
         "./structs.rkt"
         "./metrics.rkt")

(define-syntax-rule (detailed-struct name (field ...) options ...)
  (struct name (field ...) options ...
    #:methods gen:custom-write
    [(define write-proc
       (make-constructor-style-printer
        (lambda (_) 'name)
        (lambda (obj)
          (match-define (name field ...) obj)
          (list (cons 'field field) ...))))]))

(define-syntax-rule (activate-switch f)
  (begin (f) (set! f (thunk (void)))))

(define-syntax-rule (print-counter cnter p expr)
  (for ([p (counter->list cnter)] #:when (and (positive? (cdr p))
                                              expr))
    (printf "~a ~a\n" (car p) (cdr p))))

;; make an occurrence counter which is a hash from value to nat
(define/contract (counter xs) (list? . -> . hash?)
  (aggregate xs values (λ (v k x) (add1 v)) 0))

(define/contract (counter->list cnter) (hash? . -> . list?)
  (sort (hash->list cnter) > #:key cdr))

(define (aggregate xs accessor updater default #:init [init #f])
  (define h (or init (make-hash)))
  (for ([x xs])
    (define k (accessor x))
    (hash-update! h k (λ (v) (updater v k x)) default))
  h)

(define (rewire a b v)
  (if (equal? a v) b v))

(define (safe-take lst n)
  (take lst (min n (length lst))))


(define/contract (extract-score val fld) (list? symbol? . -> . any/c)
  (list-ref (profiler-row-columns val) (index-of metrics fld)))

;; callsite might be #f for builtin functions
(define/contract (callsite->file s) ((or/c string? #f) . -> . (or/c string? #f))
  (match s
    [(pregexp #px"(.*?):\\d+:\\d+" (list _ path)) path]
    [_ #f]))

(module+ test
  (require rackunit)
  (check-equal? (counter '(3 1 2 1 5))
                (make-hash '((1 . 2) (2 . 1) (3 . 1) (5 . 1))))

  (check-equal? (safe-take '(1 2 3) 4) '(1 2 3))
  (check-equal? (safe-take '(1 2) 1) '(1)))

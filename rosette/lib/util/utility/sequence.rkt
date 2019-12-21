#lang racket/base

(provide (all-defined-out))
(require racket/stream
         racket/generator
         racket/sequence
         racket/list)

(module+ test
  (require rackunit))

(define (yield-from xs)
  (for ([x xs]) (yield x)))

;; Status: DONE
;; sequence-remove-index :: (sequence/c 'a) number? ->
;;                           (cons/c (or/c 'a #f) (sequence/c 'a))
(define (sequence-remove-index xs i)
  (define xs* (sequence->generator xs))
  (define prefix (for/list ([x (in-producer xs* void?)] [limiter (in-range i)]) x))
  (define e (xs*))
  (cond
    [(void? e) (values #f prefix)]
    [else (values e (in-sequences prefix (in-producer xs* void?)))]))

;; Status: DONE
;; in-spread :: (sequence/c (sequence/c 'a)) -> (sequence/c 'a)
(define (in-spread xs)
  (define xs* (for/stream ([x xs]) (sequence->generator x)))
  (in-generator
   (let loop ([progress? #t])
     (when progress?
       (loop
        (for*/fold ([found? #f])
                   ([x (in-stream xs*)]
                    [result (in-value (x))]
                    #:when (not (void? result)))
          (yield result)
          #t))))))

;; Status: DONE
;; in-zigzag :: (sequence/c (sequence/c 'a)) -> (sequence/c 'a)
(define (in-zigzag xs)
  (let ([xs (for/stream ([x xs]) (sequence->generator x))])
    (in-generator
     (for ([limit (in-naturals 1)])
       (define changed? #f)
       (define current-idx -1)
       (for ([sub-xs (in-stream xs)] [i (in-range limit)])
         (set! current-idx i)
         (define item (sub-xs))
         (when (not (void? item))
           (yield item)
           (set! changed? #t)))
       #:break (and (not changed?) (> (sub1 limit) current-idx))
       (void)))))

(module+ test
  (check-equal? (sequence->list
                 (in-zigzag '((1 2 3)
                              (4 5 6 7)
                              (8 9)
                              (10)
                              ()
                              (11 12 13 14 15)
                              ())))
                '(1 2 4 3 5 8 6 9 10 7 11 12 13 14 15))
  (test-begin
    (define xs (for/stream ([x (in-naturals)]) (in-range x)))
    (check-equal? (for/list ([x (in-zigzag xs)] [limiter (in-range 10)]) x)
                  '(0 0 1 0 1 0 2 1 0 2)))
  (check-equal? (for/list ([x (in-zigzag (list (in-naturals) (in-naturals)))]
                           [limiter (in-range 5)]) x)
                '(0 1 0 2 1))
  (check-equal? (sequence->list (in-zigzag '(() () () (1 2 3))))
                '(1 2 3)))


(module+ test
  (test-begin
    (define-values (x rst) (sequence-remove-index '(1 2 3) 0))
    (check-equal? x 1)
    (check-equal? (sequence->list rst) '(2 3)))

  (test-begin
    (define-values (x rst) (sequence-remove-index '(1 2 3) 1))
    (check-equal? x 2)
    (check-equal? (sequence->list rst) '(1 3)))

  (test-begin
    (define-values (x rst) (sequence-remove-index '(1 2 3) 3))
    (check-equal? x #f)
    (check-equal? (sequence->list rst) '(1 2 3))))

(module+ test
  (check-equal? (sequence->list
                 (in-spread '((1 2 3)
                              (4 5 6 7)
                              (8 9)
                              (10)
                              ()
                              (11 12 13 14 15)
                              ())))
                '(1 4 8 10 11 2 5 9 12 3 6 13 7 14 15))
  (test-begin
    (define xs (for/stream ([x (in-naturals)]) (in-range x)))
    (check-equal? (for/list ([x (in-spread xs)] [limiter (in-range 5)]) x)
                  '(0 0 0 0 0)))
  (check-equal? (for/list ([x (in-spread (list (in-naturals) (in-naturals)))]
                           [limiter (in-range 5)]) x)
                '(0 0 1 1 2))
  (check-equal? (sequence->list (in-spread '(() () () (1 2 3))))
                '(1 2 3)))

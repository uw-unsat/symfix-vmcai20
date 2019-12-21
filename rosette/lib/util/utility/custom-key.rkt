#lang racket/base

(require racket/splicing
         racket/dict
         racket/function
         racket/set
         syntax/parse/define
         syntax/parse
         (for-syntax racket/base racket/string racket/syntax)
         (for-meta 2 racket/base syntax/parse))

(begin-for-syntax
  ;; match an identifier, and additionally bind
  ;; each identifier after :: to an identifier of the same name, except that
  ;; base inside each identifier is replaced with the value of
  ;; the matching identifier
  (define-syntax ~ids
    (pattern-expander
     (syntax-parser
       #:datum-literals (::)
       [(_ base :: ids ...)
        #`(~and (~var id identifier)
                (~bind [ids (let* ([full-str (symbol->string (syntax->datum #'ids))]
                                   [find-str #,(symbol->string (syntax->datum #'base))]
                                   [replaced (string-replace full-str find-str "~a")])
                              (format-id #'id replaced #'id))]) ...)]))))

(module+ test
  (require rackunit)

  (test-begin
    (define-simple-macro (test (~ids foo :: foo foo* foo-values))
      (foo ([x 1])
           (foo* ([y 2]
                  [y 3])
                 (foo-values ([(w z) (values 4 5)])
                             (list x y w z)))))

    (check-equal? (test let) (list 1 3 4 5))))

(define proc-cache (make-hash))

(define-simple-macro (define+provide-lift (~ids id ::
                                                id
                                                define-id-types
                                                define-id-types*
                                                make-id*
                                                make-immutable-id*
                                                make-weak-id*))
  (begin
    (provide define-id-types*)
    (define-simple-macro (define-id-types* name:identifier key:expr)
      (splicing-let ([KEY key])
        (define-id-types name
          (λ (x y rec) (rec (KEY x) (KEY y)))
          (λ (x rec) (rec (KEY x)))
          (λ (x rec) (rec (KEY x))))))
    (define-simple-macro (define-accessory to-define:identifier prefix:identifier)
      #:with full-id (format-id #'tmp-name "~a-~a" #'prefix #'tmp-name)
      (begin
        (provide to-define)
        (define (to-define #:key key . xs)
          (define proc (hash-ref! proc-cache (cons key 'to-define)
                                  (thunk
                                   (define-id-types* tmp-name key)
                                   full-id)))
          (apply proc xs))))
    (define-accessory make-id* make-mutable)
    (define-accessory make-immutable-id* make-immutable)
    (define-accessory make-weak-id* make-weak)))

(define+provide-lift custom-hash)
(define+provide-lift custom-set)

(module+ test

  (define (test-mutable h)
    (dict-set! h '(1 . 2) 3)
    (dict-set! h '(2 . 3) 4)
    (dict-set! h '(1 . 4) 5)
    (check-equal? (dict-ref h '(1 . 6)) 5)
    (check-equal? (dict-ref h '(2 . 7)) 4))

  (define (test-immutable h)
    (let* ([h (dict-set h '(1 . 2) 3)]
           [h (dict-set h '(2 . 3) 4)]
           [h (dict-set h '(1 . 4) 5)])
      (check-equal? (dict-ref h '(1 . 6)) 5)
      (check-equal? (dict-ref h '(2 . 7)) 4)))

  (define-custom-hash-types* car-hash car)

  (test-mutable (make-mutable-car-hash))
  (test-mutable (make-custom-hash* #:key car))

  (test-immutable (make-immutable-car-hash))
  (test-immutable (make-immutable-custom-hash* #:key car))

  (check-equal?
   (set-intersect (make-immutable-custom-set* '((1 2) (3 4)) #:key car)
                  (make-immutable-custom-set* '((3 5) (6 7)) #:key car))
   (make-immutable-custom-set* '((3 4)) #:key car))

  (test-begin
   (define s (make-custom-set* '((1 2) (3 4)) #:key car))
   (set-intersect! s (make-custom-set* '((3 5) (6 7)) #:key car))
   (check-equal? s (make-custom-set* '((3 4)) #:key car)))

  (check-equal?
   (set-union (make-immutable-custom-set* '((1 2) (3 4)) #:key car)
              (make-immutable-custom-set* '((3 5) (6 7)) #:key car))
   (make-immutable-custom-set* '((1 2) (3 4) (6 7)) #:key car))

  (test-begin
    (define s (make-custom-set* '((1 2) (3 4)) #:key car))
    (set-union! s (make-custom-set* '((3 5) (6 7)) #:key car))
    (check-equal? s (make-custom-set* '((1 2) (3 4) (6 7)) #:key car))))

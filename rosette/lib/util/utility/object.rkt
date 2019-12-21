#lang racket/base

(provide object with-object ref)
(require racket/hash
         racket/match
         racket/splicing
         racket/function
         syntax/parse/define
         "./section.rkt"
         "./macro.rkt"
         (for-syntax racket/base
                     racket/function))

(begin-for-syntax
  (define-syntax-class key-cls
    #:attributes (val static-id)
    (pattern val*:id
             #:attr val #''val*
             #:attr static-id #'val*)
    (pattern [val:expr]
             #:attr static-id #f))

  (define-syntax-class item-cls
    #:attributes (key expr static-id)
    (pattern key*:id
             #:attr expr #'key*
             #:attr key #''key*
             #:attr static-id #'key*)
    (pattern [key*:key-cls expr:expr]
             #:attr key #'key*.val
             #:attr static-id (attribute key*.static-id))))

(define union-hash (curry hash-union #:combine (λ (a b) b)))

(section "with-object"
  (define-syntax-parser with/core
    [(_ () _ _) #'(begin)]
    [(_ ([{~literal ...} rst:expr]) rst-obj obj) #'(match-define rst rst-obj)]
    [(_ (item:item-cls rst ...) rst-obj obj)
     #'(splicing-let ([KEY item.key])
         (match-define item.expr (hash-ref obj KEY))
         (with/core (rst ...) (hash-remove rst-obj KEY) obj))])

  (define-macro (with-object (item ...) obj:expr)
    (when (eq? 'expression (syntax-local-context))
      (raise-syntax-error 'with-object "can't use in an expression contewith"
                          this-syntax))
    #'(splicing-let ([OBJ obj]) (with/core (item ...) OBJ OBJ))))

(section "object"
  (define-macro (object . items)
    (define orig-stx this-syntax)
    (let lp ([items #'items] [h #'(hash)] [static-ids '()])
      (syntax-parse items
        [() (cond
              [(check-duplicate-identifier static-ids)
               => (curry raise-syntax-error 'object "duplicate static field" orig-stx)]
              [else h])]
        [([{~literal ...} e:expr] . xs) (lp #'xs #`(union-hash #,h e) static-ids)]
        [(item:item-cls . xs)
         (lp #'xs #`(hash-set #,h item.key item.expr)
             (cond
               [(attribute item.static-id) => (curryr cons static-ids)]
               [else static-ids]))]))))

(define ref hash-ref)

(module+ test
  (require rackunit)
  (test-begin
    (define d 4)
    (define base-1 (object [x '((10))] [b 20]))
    (define base-2 (object [y 30] [a 40]))
    (define obj (object
                 [a 1]
                 [... base-1]
                 [b 2]
                 [... base-2]
                 [[(string->symbol "c")] 3]
                 d))
    (let ()
      (with-object ([[(string->symbol "a")] f]
             c
             d
             [x (list (list x))]
             [... rst])
            obj)
      (check-equal? f 40)
      (check-equal? c 3)
      (check-equal? d 4)
      (check-equal? x 10)
      (check-equal? rst (object [y 30] [b 2])))))

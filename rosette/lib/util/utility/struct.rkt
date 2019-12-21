#lang racket/base

(provide struct+)
(require racket/match
         syntax/parse/define
         kw-make-struct
         (for-syntax racket/base
                     racket/syntax))

(begin-for-syntax
  (define (get-struct-field op id obj)
    ;; hopefully there's no need to check for an error here b/c
    ;; struct-copy deals with errors already
    (define getter (format-id op "~a-~a" op id))
    #`(#,getter #,obj))
  (define-syntax-class field
    (pattern id:id #:with expr #'id)
    (pattern [id:id expr:expr]))
  (define-syntax-class (field-maybe-update op obj)
    (pattern id:id #:with expr #'id)
    (pattern [id:id expr:expr])
    (pattern [id:id #:update raw-expr:expr]
             #:with expr #`(raw-expr #,(get-struct-field op #'id obj)))))

(define-simple-macro (struct+ name:id . xs)
  #:with name+ (format-id #'name "~a+" #'name)
  #:with ooo (quote-syntax ...)
  (begin
    (struct name . xs)
    (define-match-expander name+
      (syntax-parser
        [(_ :field ooo) #'(struct* name ([id expr] ooo))])
      (syntax-parser
        [(_ :field ooo) #'(make/fld name [id expr] ooo)]
        [(_ #:base obj:expr (~var f (field-maybe-update #'name #'obj)) ooo)
         #'(struct-copy name obj [f.id f.expr] ooo)]))))

(module+ test-helper
  ;; separate definition to another submodule to make sure that it doesn't require
  ;; things like `foo` to be provided
  (provide foo+ foo-bar foo-baz)
  (define (my-equal a b rec)
    (and (rec (foo-bar a) (foo-bar b))
         (rec (foo-baz a) (foo-baz b))))
  (define (my-hash a rec)
    (rec (list (foo-bar a) (foo-baz a))))
  (struct+ foo (bar baz)
           #:methods gen:equal+hash
           [(define equal-proc my-equal)
            (define hash-proc  my-hash)
            (define hash2-proc my-hash)]))

(module+ test
  (require rackunit
           (submod ".." test-helper))
  (check-equal? (foo+ [bar 1] [baz 2]) (foo+ [baz 2] [bar 1]))
  (let ([bar 1]) (check-equal? (foo+ bar [baz 2]) (foo+ [bar 1] [baz 2])))
  (check-equal? (foo+ #:base (foo+ [bar 1] [baz 2]) [baz 3]) (foo+ [bar 1] [baz 3]))
  (match-let ([(foo+ [bar x] baz) (foo+ [bar 1] [baz 2])])
    (check-equal? x 1)
    (check-equal? baz 2))
  (check-equal? (foo+ #:base (foo+ [bar 1] [baz 2]) [baz #:update (λ (x) (* 3 x))])
                (foo+ [bar 1] [baz 6])))

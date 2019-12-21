#lang racket/base

(provide (all-defined-out))
(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser cond*
  [(_ [#:do whatever ...] . clause)
   #'(let ()
       whatever ...
       (cond* . clause))]
  [(_ [#:let x:id e:expr] . clause)
   #'(let ([x e]) (cond* . clause))]
  [(_ [{~literal else} . body])
   #'(let () . body)]
  [(_ [test:expr {~literal =>} e:expr] . clause)
   #'(let ([t test]) (if t (e t) (cond* . clause)))]
  [(_ [test:expr . body] . clause)
   #'(if test (let () . body) (cond* . clause))]
  [(_) (error 'cond* "non-exhaustive cond")])

;; just like binding in maybe monad
(define-syntax-parser and=>
  [(_) #'(and)]
  [(_ e1:expr) #'(and e1)]
  [(_ e1:expr (~literal =>) e-next:expr e2:expr ...)
   #'(let ([it e1]) (and=> it (e-next it) e2 ...))]
  [(_ e1:expr e2:expr ...) #'(and e1 (and=> e2 ...))])

(module+ test
  (require racket/function
           rackunit)

  (check-equal? (and=> 5 => (curry + 3) => (curry + 1)) 9)
  (check-equal? (and=> 5 => (const #f) => (curry + 1)) #f))

#lang racket/base

(provide (all-defined-out))
(require racket/match
         syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser %
  [(_ e:expr ...+ {~and k {~not :expr}} rst ...) #'(let () e ... (% k rst ...))]
  [(_ #:if [test-expr:expr then ...+] rst ...+)
   #'(if test-expr (% then ...) (% rst ...))]
  [(_ #:with [pat e:expr] rst ...) #'(match-let ([pat e]) (% rst ...))]
  [(_ e:expr ...+) #'(let () e ...)])

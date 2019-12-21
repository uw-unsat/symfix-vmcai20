#lang racket/base

(provide (all-defined-out))
(require racket/struct
         racket/match
         rosette/lib/util/utility/main)

(define (format-maybe-syntax x)
  (if (syntax? x)
      (unquoted-printing-string (pretty-syntax-format x))
      x))

(struct+ advice-serval-hack (target) #:transparent
         #:methods gen:custom-write
         [(define write-proc
            (make-constructor-style-printer
             (λ (obj) 'advice-serval-hack)
             (λ (obj)
               (match-define (advice-serval-hack target) obj)
               (list (format-maybe-syntax target)))))])

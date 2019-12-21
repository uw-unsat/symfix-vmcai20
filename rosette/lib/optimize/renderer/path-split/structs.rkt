#lang racket/base

(provide (all-defined-out))
(require racket/struct
         racket/match
         rosette/lib/util/utility/main)

(define (format-maybe-syntax x)
  (if (syntax? x)
      (unquoted-printing-string (pretty-syntax-format x))
      x))

(struct+ advice-path-split (forall target type) #:transparent
         #:methods gen:custom-write
         [(define write-proc
            (make-constructor-style-printer
             (λ (obj) 'advice-path-split)
             (λ (obj)
               (match-define (advice-path-split forall target type) obj)
               (list (format-maybe-syntax forall)
                     (format-maybe-syntax target)
                     type))))])

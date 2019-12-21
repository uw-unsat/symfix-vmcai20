#lang racket/base

(require syntax/parse/define
         racket/stxparam
         racket/match
         racket/format
         (for-syntax racket/base))

(provide (all-defined-out))

(define-syntax-parameter who
  (λ (stx) (raise-syntax-error (syntax-e stx) "use out of context")))

(define-simple-macro (define-reporter name:id
                       [(matching-msg patterns ...) body:expr ...+] ...)
  (define (name msg . payload)
    (case msg
      [(matching-msg) (syntax-parameterize ([who (make-rename-transformer #'msg)])
                        (match-define (list patterns ...) payload)
                        (let () body ...))] ...
      [else
       (displayln
        (~a "message sent to the reporter " msg
            " does not match the record with the payload " payload))])))

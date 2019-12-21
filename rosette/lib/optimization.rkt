#lang racket/base

(require (for-syntax racket/base))

(module local-mod racket/base
  (provide (all-defined-out))
  (require rosette/lib/util/utility/main)

  (define SAFE 'safe)
  (define UNSAFE 'unsafe)

  (define current-progress? (make-parameter #f))

  (define (clear-safe stx)
    (syntax-property stx 'symfix:safe? #f))

  (define (safe? stx)
    (define x (syntax-property stx 'symfix:safe?))
    (cond
      [(eq? x SAFE) #t]
      [(eq? x UNSAFE) #f]
      [else (error 'safe? "syntax ~a doesn't have symfix:safe? property" stx)]))

  (define (set-syntax-safe stx b)
    (define previous (syntax-property stx 'symfix:safe?))
    (define next (if b SAFE UNSAFE))
    (cond
      [(eq? previous next) stx]
      [previous (error 'set-syntax-safe "syntax ~a overrides safety ~a" stx b)]
      [else (current-progress? #t)
            (syntax-property stx 'symfix:safe? next)]))

  (define (set-safe? stx) (syntax-property stx 'symfix:safe?)))

(require 'local-mod
         (for-syntax (only-in 'local-mod set-syntax-safe)))

(provide (all-from-out 'local-mod)
         safe)
(define-syntax (safe stx)
  (syntax-case stx ()
    [(_ form) (set-syntax-safe #'form #t)]))

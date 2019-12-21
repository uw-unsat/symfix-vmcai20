#lang racket/base

(provide current-reporter)
(require "../../tools/reporter.rkt")

(define-reporter default-reporter
  [(deforest/inline-id id rhs) (void)]
  [(deforest/optimized old-stx new-stx)
   (printf "> deforest ~a to ~a\n" old-stx new-stx)]
  [(deforest/optimized-ref stx)
   (printf "> deforest accessors ~a\n" stx)]
  [(deforest/inline-app/succeed lam args) (void)]
  [(deforest/inline-app/fail lam args) (void)])

(define current-reporter (make-parameter default-reporter))

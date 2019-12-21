#lang racket/base

(provide (all-defined-out))
(require syntax/parse/define
         racket/match)

(define-simple-macro (with pat:expr e:expr) (match-define pat e))

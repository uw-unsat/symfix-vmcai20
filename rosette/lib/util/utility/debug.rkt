#lang racket/base

(provide (all-defined-out))
(require racket/format
         racket/pretty
         syntax/parse/define
         terminal-color
         "./define.rkt"
         "./syntax/pretty-syntax-format.rkt"
         (for-syntax racket/base))

(define-multiple
  #:prefix current-debug-
  #:trans (make-parameter)
  [fg 'red]
  [bg 'default])

(define (debug/core x msg-backup
                    #:msg [msg #f]
                    #:fg [fg (current-debug-fg)]
                    #:bg [bg (current-debug-bg)])
  (displayln
   (format "~a: ~a"
           (or msg msg-backup)
           (cond
             [(syntax? x) (pretty-syntax-format x)]
             [else (pretty-format x)])))
  x)

(define-simple-macro (debug: x opt ...)
  (debug/core x 'x opt ...))

(define (assert x #:msg [msg #f] . ctx)
  (when (not x)
    (displayln (format "ASSERTION ERROR~a" (or (~a ": " msg) "")))
    (for ([e ctx])
      (displayln (format "% ~a" (pretty-format e))))
    (error 'assert)))

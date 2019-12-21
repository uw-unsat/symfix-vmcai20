#lang racket/base

(provide main)
(require racket/match
         racket/sandbox
         racket/string
         racket/exn
         racket/function
         syntax/parse
         rosette/lib/util/utility/main
         (only-in rosette/safe term-cache clear-state!)
         rosette/lib/syntax-utils/utils
         rosette/lib/optimize/compile
         rosette/lib/optimize/tool
         rosette/lib/optimize/renderer/optimize
         rosette/lib/optimize/struct)

(define (main f-info transformer time-limit instant? dump load?)
  (match-define (mod-info+ mod mod-pretty filename) f-info)

  (define info #f)
  (define (run-install)
    (parameterize ([current-compile (make-symbolic-profile-compile-handler
                                     #:transformer transformer
                                     #:dump dump
                                     #:load? load?
                                     #:instant? instant?)])
      (with-handlers ([exn:fail:resource? (λ (e) (set! info `(timeout ,e)))]
                      [exn:fail? (λ (e) (set! info `(exception ,e)))])
        (match-define-values (_ _ real _)
          (with-limits time-limit #f
            (time-apply (thunk (dynamic-require mod #f)) '())))
        (set! info real))))

  (define result
    (profile-thunk run-install
                   #:renderer make-optimize-renderer
                   #:source mod-pretty
                   #:name (format "~a" filename)))

  (clear-state!)

  (match info
    [`(timeout ,e)
     (match-define (double profile-rows main-row) result)
     (experiment-result+
      profile-rows
      main-row
      [duration 'timeout])]
    [`(exception ,e)
     (define s (exn->string e))
     (when (not (string-contains? s "expand: namespace mismatch"))
       (displayln s))
     (exception+ e)]
    [_
     (match-define (double profile-rows main-row) result)
     (experiment-result+
      profile-rows
      main-row
      [duration (quotient info 1000)])]))

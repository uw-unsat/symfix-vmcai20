#lang racket/base

(provide (all-defined-out))

(define (module-to-profile file mod)
  (define file-path `(file ,file))
  (define mod-path `(submod ,file-path ,mod))
  (if (module-declared? mod-path #t)
      (values mod-path mod-path)
      (values file-path file)))

#lang racket/base

(require racket/list
         racket/match
         racket/string
         racket/set
         racket/generator
         syntax/stx
         rosette/lib/util/utility/main
         "./transformer.rkt"
         "../structs.rkt"
         "../../struct.rkt"
         "../../optimize-reporter.rkt")

(provide advise-value-split)

(define (report msg . payload) (void))

(struct+ advice-value-split (name) #:transparent)

(define (advise-value-split rows stxs)
  (define acc '())
  (define (process-row the-row)
    (match the-row
      [(profiler-row+ [function f]
                      columns
                      score
                      parent)
       (for ([row (in-list parent)]) (process-row row))
       (when (string-prefix? f "struct:")
         (report 'struct:found f columns)
         (set! acc
               (cons
                (double (string->symbol (substring f (string-length "struct:")))
                        score)
                acc)))]))
  (for-each process-row rows)
  (define structs (sort acc > #:key snd))
  (in-generator
   (for ([row structs])
     (with (double function _) row)
     (yield
      (advice+
       [transformer (transform-value-split function)]
       [explanation (advice-value-split function)])))))

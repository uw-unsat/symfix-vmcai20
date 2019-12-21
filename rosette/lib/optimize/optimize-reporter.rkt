#lang racket/base

(provide (all-defined-out))

(require racket/pretty
         racket/exn
         racket/date
         rosette/lib/util/utility/main
         "./renderer/structs.rkt"
         "../../tools/reporter.rkt")


(define-reporter default-reporter

  [(init) (printf "> begin optimization\n")]
  [(real-flight/elide)
   (printf "> real flight elided\n")]
  [(begin/experiment)
   (printf "\n\n=== new experiment on ~a ===\n" (date->string (current-date) #t))]

  [(struct-compress picked) (printf "> struct: ~a\n" picked)]










  [(profiler/result rows)
   (void)
   #;(printf "> profiler rows: ~a\n" (pretty-format (map profiler-row->short-row rows)))]

  [(struct-compress/transform/init name) (printf "> struct-compress at ~a\n" name)]

  [(begin/memoization) (printf "> test memoization flight\n")]
  [(finish/memoization cpu real) (printf "> memoization cpu: ~a real: ~a\n" cpu real)]

  [(union/advise/init) (void)]
  [(union/advise/find-uexpr/failure source line ch)
   (printf "> failed to find the matching union application at ~a line ~a ch ~a\n"
           source line ch)]
  [(union/advise/elide adv)
   (printf "> advice elided\n")]

  [(union/transform/init mode forall target)
   (printf "> track: ~a\n" mode)
   (printf "> inserted for/all at file ~a line ~a ch ~a targeting at ~a\n"
           (syntax-source forall)
           (syntax-line forall)
           (syntax-column forall)
           (syntax->datum target))]
  [(union/transform/begin source)
   (printf "> transform file: ~a\n" source)]
  [(union/transform/subst stx)
   (printf "> substituted at line ~a ch ~a\n"
           (syntax-line stx)
           (syntax-column stx))]

  [(deforestation/result result)
    (printf "> deforestation should be enabled?: ~a\n" result)]
  [(begin/test-flight)
    (printf "> test flight\n")]
  [(exception e)
   (printf "> exception: ~a" (exn->string e))]
  [(timeout)
   (printf "> timeout\n")]

  )

(define current-reporter (make-parameter default-reporter))

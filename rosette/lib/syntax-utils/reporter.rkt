#lang racket/base

(provide current-reporter)
(require #;(only-in "../optimization.rkt" safe?)
         terminal-color
         "../../tools/reporter.rkt")

(define-reporter default-reporter
  [(safety-analysis/start module-name)
   (void)
   #;(printf "> safety-analysis starts at module ~a\n" module-name)]
  [(safety-analysis/no-progress)
   (void)
   #;(printf "> safety-analysis has no progress\n")]
  [(safety-analysis/new-loop)
   (void)
   #;(printf "> safety-analysis enters new loop\n")]
  [(safety-analysis/end)
   (void)
   #;(printf "> safety-analysis ends\n")]
  [(safety-analysis/define/safe val)
   (void)
   #;(printf "> safety-analysis: ~a is safe\n" val)]
  [(safety-analysis/define-values/safe val)
   (void)
   #;(printf "> safety-analysis: ~a is safe\n" val)]
  [(safety-analysis/define/safe-at-fixpoint val)
   (void)
   #;(printf "> safety-analysis: ~a is safe at fixpoint\n" val)]
  [(safety-analysis/define-values/safe-at-fixpoint val)
   (void)
   #;(printf "> safety-analysis: ~a is safe at fixpoint\n" val)])

(define current-reporter (make-parameter default-reporter))

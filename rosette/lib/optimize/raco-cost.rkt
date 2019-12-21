#lang racket/base

(require racket/cmdline
         racket/match
         "./common.rkt"
         (only-in "./measure-worker.rkt" [main run])
         "./renderer/structs.rkt"
         "./struct.rkt")


(define current-out-path (make-parameter #f))
(define current-module-name (make-parameter 'main))
(define file
  (command-line
   #:once-each
   [("--out") out "Output path" (current-out-path out)]
   #:args (filename)
   filename))

(when (not (file-exists? file))
  (printf "file ~a doesn't exist\n" file)
  (exit 1))

(define-values (mod mod-pretty)
  (module-to-profile file (current-module-name)))

(define (main out)
  (match (run (mod-info+ mod mod-pretty [filename file]) values #f #f #f #f)
    [(experiment-result+ main-row profile-rows [duration dur])
     (cond
       [(eq? dur 'timeout) (writeln #f out)]
       [else (match (profiler-row-columns-incl main-row)
               [(list a b c d) (writeln (list a c d) out)])])]))

(cond
  [(current-out-path) (call-with-output-file* (current-out-path) #:mode 'text #:exists 'replace main)]
  [else (main)])

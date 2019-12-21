#lang racket/base

(require racket/cmdline
         racket/function
         raco/command-name
         rosette/lib/util/utility/main
         "./common.rkt"
         "./struct.rkt"
         "./optimizer.rkt")

(define current-module-name (make-parameter 'main))
(define current-time-limit (make-parameter 60))
(define current-cosette-hack? (make-parameter #f))
(define current-serval-hack? (make-parameter #f))
(define current-out-path (make-parameter #f))
(define current-algorithm (make-parameter "symfix"))
(random-seed 305443914)

(define file
  (command-line #:program (short-program+command-name)
                ; Tool configuration
                #:help-labels "" "Profiled code settings"
                #:once-each
                [("-m" "--module") name "Run submodule <name> (defaults to 'main)"
                                   (current-module-name (string->symbol name))]
                [("--out") out "Output path" (current-out-path out)]
                [("--time-limit") time-limit "Time limit"
                                  (current-time-limit (string->number time-limit))]
                [("--algorithm") algorithm "Algorithm" (current-algorithm algorithm)]
                [("--cosette-hack") "Enable Cosette hack" (current-cosette-hack? #t)]
                [("--serval-hack") "Enable Serval hack" (current-serval-hack? #t)]

                #:help-labels ""
                #:args (filename . args)
                ; pass all unused arguments to the file being run
                (current-command-line-arguments (list->vector args))
                filename))

(when (not (file-exists? file))
  (printf "file ~a doesn't exist\n" file)
  (exit 1))

(printf "Algorithm: ~a\n" (current-algorithm))
(printf "Time limit: ~a\n" (current-time-limit))
(printf "Output path: ~a\n" (current-out-path))
(newline)
(newline)
(newline)

(collect-garbage)
(collect-garbage)
(collect-garbage)

(define-values (mod mod-pretty)
  (module-to-profile file (current-module-name)))

(require racket/sandbox)

(with-limits (current-time-limit) #f
  (optimizer (mod-info+ mod mod-pretty [filename file])
             (current-algorithm)
             (current-cosette-hack?)
             (current-serval-hack?)
             (current-out-path)))

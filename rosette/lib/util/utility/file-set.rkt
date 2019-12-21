#lang racket/base

(provide make-file-set)
(require racket/function
         racket/format
         "./debug.rkt"
         racket/file)

(define an-executor (make-will-executor))
(void
 (thread
  (thunk
   (let loop ()
     (will-execute an-executor)
     (loop)))))

(define ((executor-proc p) v)
  (printf "INFO: removing file set ~a\n" p)
  (delete-file p))

(define (make-file-set [template "rkttmp"])
  (define p (make-temporary-file (string-append template "~a")))
  (printf "INFO: creating file set ~a\n" p)
  (define (reader v)
    (with-input-from-file p #:mode 'text
      (thunk (for/or ([v* (in-port)])
               ;; TODO(Oak): weird bug. Somehow need ~a to make equal things equal
               (equal? (~a v) (~a v*))))))
  (define (writer v)
    (with-output-to-file p #:mode 'text #:exists 'append
      (thunk (writeln v))))
  (will-register an-executor reader (executor-proc p))
  (values reader writer))

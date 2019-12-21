#lang racket/base

(provide (all-defined-out))
(require racket/set
         racket/file
         racket/string
         racket/sequence
         racket/format
         racket/list
         racket/function
         racket/match
         racket/system
         "./define.rkt")

(define-multiple
  #:prefix current-diff-
  #:trans (make-parameter)
  [context 3]
  [column 100])

(define (display-to-file* a path-a)
  (display-to-file a path-a #:mode 'text #:exists 'replace))

#;(define (display-to-file* a path-a)
  (define result
    (for/list ([line (string-split (~a a) "\n")])
      (define seq (string->list line))
      (for/list ([group (in-slice (current-diff-column) seq)])
        (list->string group))))

  (display-to-file (string-join (append* result) "\n")
                   path-a
                   #:mode 'text
                   #:exists 'replace))

(define (print-diff a b)
  (define path-a (make-temporary-file))
  (define path-b (make-temporary-file))
  (display-to-file* a path-a)
  (display-to-file* b path-b)

  (system* (find-executable-path "diff")
           "-d" "-b" "-C" "3"
           (path->string path-a)
           (path->string path-b))

  ;; (define-values (in out) (make-pipe))

  ;; (parameterize ([current-output-port out])
  ;;   )

  ;; (close-output-port out)

  ;; (delete-file path-a)
  ;; (delete-file path-b)

  ;; (define show-set (mutable-set))

  ;; (define lines
  ;;   (for/list ([line (in-lines in)] [i (in-naturals)])
  ;;     (when (not (char=? (string-ref line (sub1 (current-diff-column))) #\space))
  ;;       (for ([j (in-range (- i (current-diff-context))
  ;;                          (+ i (current-diff-context) 1))])
  ;;         (set-add! show-set j)))
  ;;     line))

  ;; (for ([line lines] [i (in-naturals)] #:when (set-member? show-set i))
  ;;   (when (not (set-member? show-set (sub1 i)))
  ;;     (displayln (make-string (* 2 (current-diff-column)) #\-)))
  ;;   (displayln line))
  ;; (when (not (set-empty? show-set))
  ;;   (displayln (make-string (* 2 (current-diff-column)) #\-)))
  )

#;(define (print-diff a b)
  (define path-a (make-temporary-file))
  (define path-b (make-temporary-file))
  (display-to-file* a path-a)
  (display-to-file* b path-b)

  (define-values (in out) (make-pipe))

  (parameterize ([current-output-port out])
    (system* (find-executable-path "sdiff")
             "-d" "-b" "-t" "-w" (number->string (* 2 (current-diff-column)))
             (path->string path-a)
             (path->string path-b)))

  (close-output-port out)

  (delete-file path-a)
  (delete-file path-b)

  (define show-set (mutable-set))

  (define lines
    (for/list ([line (in-lines in)] [i (in-naturals)])
      (when (not (char=? (string-ref line (sub1 (current-diff-column))) #\space))
        (for ([j (in-range (- i (current-diff-context))
                           (+ i (current-diff-context) 1))])
          (set-add! show-set j)))
      line))

  (for ([line lines] [i (in-naturals)] #:when (set-member? show-set i))
    (when (not (set-member? show-set (sub1 i)))
      (displayln (make-string (* 2 (current-diff-column)) #\-)))
    (displayln line))
  (when (not (set-empty? show-set))
    (displayln (make-string (* 2 (current-diff-column)) #\-))))

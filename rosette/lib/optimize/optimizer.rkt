#lang racket/base

(provide optimizer)

(require racket/match
         racket/class
         racket/port
         racket/string
         racket/pretty
         racket/function
         racket/dict
         racket/list
         racket/generator
         racket/math
         racket/format
         racket/date
         racket/runtime-path

         syntax/parse/define
         syntax/parse

         rosette/lib/util/utility/main
         terminal-color

         "./struct.rkt"
         "./renderer/structs.rkt"

         "../deforest/compiler.rkt"

         "./renderer/path-split/adviser.rkt"
         "./renderer/value-split/adviser.rkt"
         "./renderer/random/adviser.rkt"
         "./renderer/serval-hack/adviser.rkt"

         "../syntax-utils/utils.rkt"
         "../syntax-utils/safety.rkt"

         "../../tools/reporter.rkt"

         (for-syntax racket/base))

(define-runtime-path worker-path "measure-worker.rkt")

(module utils racket/base
  (provide (all-defined-out))

  (require syntax/parse/define
           racket/match
           racket/pretty
           racket/generator
           racket/list
           racket/stream
           racket/string
           rosette/lib/util/utility/main
           (for-syntax racket/base))

  ;; Status: DONE
  ;; format-fields :: (listof (list/c any/c any/c))
  (define (format-fields bindings)
    (pretty-format bindings))

  ;; Status: DONE
  ;; list<? :: (listof number?) (listof number?) -> boolean?
  (define (list<? xs ys)
    (match* (xs ys)
      [('() '()) #f]
      [((cons x xs) (cons y ys)) (or (< x y) (and (= x y) (list<? xs ys)))])))

(require 'utils)

(define-reporter default-reporter
  [(main-loop:begin-iteration fringe i memory best)
   (displayln
    (format "~a: at iteration ~a at ~a with memory ~a and queue size ~a"
            who i (date->string (current-date) #t)
            (get-gb memory) (length fringe)))
   (when best
     (printf "best so far:\n")
     (printf "[metrics ~a]\n" (send best get-metrics))
     (printf "[id ~a]\n\n" (get-field id best)))]
  [(main-loop:done) (displayln who)]
  [(main-loop:child-prune child) (displayln who)]
  [(main-loop:child-activate prog child) (displayln who)]
  [(main-loop:child-exhaust) (displayln who)]

  [(initialize:init) (displayln who)]
  [(initialize:deforest deforest?) (displayln who)]
  [(initialize:deforest-decision deforest?) (displayln who)]

  [(test-flight:regular) (displayln who)]
  [(test-flight:deforest) (displayln who)]

  [(flight:result node) (displayln who)]
  [(flight:exception e)
   (newline)
   (displayln (format "~a: ~a" who e))]
  [(flight:result-cosette columns dur) (displayln who)]
  [(gc:minor len) (printf "~a: GC-ed ~a items\n" who len)]
  [(gc:critical memory) (printf "~a: with memory ~a\n" who (get-gb memory))]

  [(candidate node) (displayln who)])

(define current-reporter (make-parameter default-reporter))

(define-simple-macro (report . body) ((current-reporter) . body))

(define current-randomizer (make-parameter #f))
(define current-pick (make-parameter #f))
(define current-diff? (make-parameter #f))
(define current-starter (make-parameter #f))

(define-values (member? record!) (make-file-set ".symfix"))

;; Status: DONE
;; prune? :: program? (or/c number? #f) -> boolean?
(define (prune? prog max-depth)
  (cond*
   [(equal? (get-field depth prog) max-depth) #t]
   [#:let repr (send prog get-representation)]
   [(member? repr) #t]
   [else (record! repr)
         #f]))

;; Status: DONE
;; default-selector :: (sequence/c 'a) -> ((or/c 'a #f) (sequence/c 'a))
;; Randomize an element from the sequence, preferring the first few elements
(define (default-selector xs)
  (define-values (child children)
    (sequence-remove-index xs ((current-randomizer))))
  (cond
    [child (values child children)]
    [else (sequence-remove-index children 0)]))

(define (default-refine xs ys) #f)

;; Status: DONE
;; program< :: program? program? -> boolean?
;; Returns #t when p1 is strictly worse than p2
(define (program</depth p1 p2 #:refine refine)
  (cond
    [(and (send p1 get-duration) (send p2 get-duration))
     ;; metrics should be available, then
     (define xs (send p1 get-metrics))
     (define ys (send p2 get-metrics))
     ;; (a, b) < (c, d) when a < c and b <= d, or a <= c and b < d
     (or (and (andmap >= xs ys) (not (equal? xs ys))
              (>= (get-field depth p1) (get-field depth p2)))
         (and (andmap >= xs ys)
              (> (get-field depth p1) (get-field depth p2)))
         (refine xs ys))]
    [(send p1 get-duration) #f]
    [(send p2 get-duration) #t]
    [else (> (get-field depth p1) (get-field depth p2))]))

(define (program< p1 p2 #:refine refine)
  (cond
    [(or (and (send p1 get-duration) (send p2 get-duration))
         #;(and (not (send p1 get-duration)) (not (send p2 get-duration))))
     ;; metrics should be available, then
     (define xs (send p1 get-metrics))
     (define ys (send p2 get-metrics))
     ;; (a, b) < (c, d) when a < c and b <= d, or a <= c and b < d
     (cond
       [(equal? xs ys)
        (> (length (send p1 get-explanations))
           (length (send p2 get-explanations)))]
       [else (or (andmap >= xs ys) (refine xs ys))])]
    [(send p1 get-duration) #f]
    [(send p2 get-duration) #t]
    [else #f]))

;; Status: DONE
;; get-penalty/one :: program? (listof program?) -> number?
(define (get-penalty/one p fringe refine)
  (count (curry program</depth p #:refine refine) fringe))

(define (default-sum-metrics xs ys) (> (apply + xs) (apply + ys)))
(define refiners (list default-refine default-sum-metrics))

;; Status: DONE
;; get-penalty :: program? (listof program?) -> number?
(define (get-penalty p fringe #:tie-breaker? [tie-breaker? #f])
  (append
   (for/list ([refine (in-list refiners)])
     (get-penalty/one p fringe refine))
   (if tie-breaker? (list (random)) (list))))

;; Status: DONE
;; pick :: (listof program?) -> program?
;; Precondition: the list is not empty
(define (pick/smart fringe)
  (define sorted-fringe (sort fringe list<? #;(if pick-reverse? (λ (x y) (list<? (reverse x) (reverse y))) list<?)
                              #:key (curryr get-penalty fringe #:tie-breaker? #t)
                              #:cache-keys? #t))
  #;(println sorted-fringe)
  (define-values (picked _)
    (default-selector (remove-duplicates sorted-fringe
                                         #:key (curryr get-penalty fringe))))
  picked)

(define (pick/symfix fringe)
  (define sorted-fringe (sort fringe (curry program< #:refine default-sum-metrics)))
  (define-values (picked _) (default-selector (reverse sorted-fringe)))
  picked)

(define (pick/greedy/depth fringe)
  (define sorted-fringe (sort fringe (curry program</depth #:refine default-sum-metrics)))
  #;(println sorted-fringe)
  (define-values (picked _) (default-selector (reverse sorted-fringe)))
  picked)

(define (pick fringe) ((current-pick) fringe))

(define skip? #f)

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (print-values . xs)
  (for ([x (in-list xs)])
    (when (not (void? x)) (println x))))

(define (interp stx)
  (match stx
    ['exit #f]
    ['exit+skip (set! skip? #t)
                #f]
    [else (call-with-values (thunk (eval stx ns)) print-values)
          #t]))

(define (breakpoint)
  (when (not skip?)
    (let loop ()
      (printf ">>> ")
      (define stx (read))
      (when (interp stx) (loop)))))

(define ((greedy-start greedy-param)
         prog
         #:max-depth [max-depth 400])
  (define best prog)

  (let loop ([iteration 0] [prog prog])
    (report 'main-loop:begin-iteration '() iteration (measure-memory) best)
    (define best-local #f)
    (for ([limiter (in-range greedy-param)])
      (define child (send prog get-child))
      #:break (not child)
      (cond
        [(prune? child max-depth) (report 'main-loop:child-prune child)]
        [else
         (report 'main-loop:child-activate prog child)
         (send child activate!)
         (when (or (not best)
                   (program< best child #:refine default-sum-metrics))
           (set! best child)
           (send child dump (add1 iteration)))
         (when (or (not best-local)
                   (program< best-local child #:refine default-sum-metrics))
           (set! best-local child))]))
    (when best-local
      (loop (add1 iteration) best-local))))

(define (start prog #:max-depth [max-depth 20])
  (define fringe (list prog))
  (define best prog)

  ;; Status: DONE
  ;; gc :: fringe? -> fringe?
  ;; garbage collect the fringe list
  (define (gc)
    (define-values (done new-fringe)
      (partition (λ (prog) (eq? 'done (send prog get-state))) fringe))
    (when (not (empty? done))
      (report 'gc:minor (length done))
      (for ([done-e (in-list done)])
        (match (get-penalty done-e fringe)
          [(list 0 _) (report 'candidate done-e)]
          [_ (void)]))
      (set! fringe new-fringe))

    (when (> (measure-memory) (* 10 1024 1024 1024))
      (report 'gc:critical (measure-memory))
      (define sorted-fringe (sort fringe list<?
                                  #:key (curryr get-penalty fringe)
                                  #:cache-keys? #t))
      (set! fringe (take sorted-fringe (quotient (length sorted-fringe) 2)))))

  (let loop ([iteration 0])
    (gc)
    (report 'main-loop:begin-iteration fringe iteration (measure-memory) best)
    (cond*
     [(empty? fringe) (report 'main-loop:done)]
     [#:let prog (pick fringe)]
     [#:let child (send prog get-child)]
     [(and child (prune? child max-depth))
      (report 'main-loop:child-prune child)
      (loop iteration)]
     [child
      (report 'main-loop:child-activate prog child)
      (send child activate!)
      (when (or (not best) (program< best child #:refine default-sum-metrics))
        (set! best child)
        (send child dump (add1 iteration)))
      (set! fringe (cons child fringe))
      (loop (add1 iteration))]
     [else
      (report 'main-loop:child-exhaust)
      (loop iteration)])))

;; Status: DONE
;; make-collect-transformer :: -> ((-> (listof (list/c string? syntax? number?)))
;;                                 transformer?)
(define cache-collector (make-hash))
(define (make-collect-transformer)
  (define src '())
  (values
   (λ (#:clear? [clear? #f])
     (begin0 src
       (when clear? (set! src '()))))
   (λ (stx phase)
     (define val
       (hash-ref! cache-collector
                  (list (syntax->string-path stx)
                        (syntax->datum stx)
                        phase)
                  (thunk (list (syntax->string-path stx) stx phase))))
     (set! src (cons val src))
     (values (second val) phase))))

(define (transform-expand stx phase)
  (values (expand-syntax stx) phase))

;; Status: DONE
;; NOTE: make sure that parameterizing current-namespace is enough
;;       and that we won't need custom-load
;; make-runner :: program-info? -> ((syntax? -> syntax?) (or/c number? #f) ->
;;                                  (or/c experiment-info? exception?))
(define ((make-runner prog-info this-directory)
         transformer
         #:time-limit [time-limit #f]
         #:instant? [instant? #f]
         #:load? [load? #f]
         #:dump [dump #f])
  (current-directory this-directory)
  (dict-clear! symbolic-store)
  (begin0 (parameterize ([current-namespace (make-base-namespace)])
            ((dynamic-require `(file ,(path->string worker-path)) 'main)
             prog-info transformer time-limit instant? dump load?))
    (current-directory this-directory)))

(define current-random-base (make-parameter (exp 1)))

(define (random-pos) (exact-floor (- (log (random) (current-random-base)))))

(define (optimizer prog-info algorithm cosette-hack? serval-hack? log-path)
  (when log-path
    (with-output-to-file log-path
      #:exists 'replace #:mode 'text
      (thunk (writeln #f))))
  (current-randomizer (thunk* 0))
  (define all-advisers
    (match algorithm
      ["symfix" (current-pick pick/symfix)
                (current-starter start)
                (append (list advise-path-split)
                        (if serval-hack? (list advise-serval-hack) (list))
                        (list advise-value-split)
                        (list 'deforestation))]

      ["greedy" (current-starter (greedy-start 1))
                (append (list advise-path-split)
                        (if serval-hack? (list advise-serval-hack) (list))
                        (list advise-value-split)
                        (list 'deforestation))]
      ["random" (current-pick pick/symfix)
                (current-starter start)
                (list advise-random)]))

  (optimizer/one prog-info all-advisers cosette-hack? algorithm log-path))

(define (optimizer/one prog-info
                       pre-advisers
                       cosette-hack?
                       algorithm
                       log-path)
  (define run (make-runner prog-info (current-directory)))
  (define time-limit #f)
  (define current-global-id 1)
  (define advisers (filter-not symbol? pre-advisers))

  (define program%
    (class* object% [printable<%>]
      (super-new)
      (init-field
       ;; (listof (list/c string? syntax? number?))
       src
       ;; number?
       depth
       ;; number?
       sibling
       ;; number?
       id
       ;; list?
       explanations)

      ;; either 'fresh, 'activated, or 'done
      (define state 'fresh)

      (define metrics #f)
      (define duration #f)
      (define children #f)

      ;; Status: DONE
      ;; get-state :: -> status?
      (define/public (get-state) state)

      ;; Status: DONE
      ;; ensure-not-fresh! :: symbol? -> void?
      (define (ensure-not-fresh! who)
        (when (eq? state 'fresh)
          (raise-user-error
           who
           "invariant violation: method called before activate!")))

      ;; Status: DONE
      ;; get-representation :: -> (listof (list/c string? datum-syntax?))
      (define/public (get-representation)
        (sort (for/list ([item (in-list src)])
                (list (first item) (~a (syntax->datum (second item)))))
              string<?
              #:key first))

      ;; Status: DONE
      ;; get-duration :: -> (or/c number? #f)
      (define/public (get-duration)
        (ensure-not-fresh! 'get-duration)
        duration)

      ;; Status: DONE
      ;; get-metrics :: -> (or/c (listof number?) #f)
      (define/public (get-metrics)
        (ensure-not-fresh! 'get-metrics)
        metrics)

      ;; Status: DONE
      ;; get-child :: -> (or/c program? #f)
      (define/public (get-child #:selector [selector default-selector])
        (ensure-not-fresh! 'get-child)
        (define-values (child new-children) (selector children))
        (set! children new-children)
        (when (not child) (set! state 'done))
        child)

      ;; Status: DONE
      ;; get-explanations :: -> list?
      (define/public (get-explanations) explanations)

      ;; Status: DONE
      ;; transform-load :: transformer?
      (define (transform-load stx phase)
        (match (assoc (syntax->string-path stx) src)
          [(list _ expanded-stx phase) (values expanded-stx phase)]
          [_ (error 'transform-load "~a: ~a" (syntax->string-path stx) (map first src))]))

      ;; Status: DONE
      ;; activate! :: -> void?
      (define/public (activate! #:diff? [diff? #f]
                                #:print-profile? [print-profile? #f])
        (set! state 'activated)
        (match (run transform-load #:time-limit time-limit #:load? #t)
          [(experiment-result+ main-row profile-rows [duration dur])
           #:when (or (eq? dur 'timeout) (not cosette-hack?)
                      (>= (apply + (profiler-row-columns-incl main-row)) 5000))

           (define dur* (and (not (eq? dur 'timeout)) dur))
           (define sibling-cnt 0)
           (when (and dur* (not time-limit))
             (set! time-limit (+ 10 (* 1.2 dur*))))
           (set! duration dur*)
           (set! metrics (profiler-row-columns-incl main-row))
           (set! children
                 (in-spread
                  (for/list ([advise (in-list (shuffle advisers))])
                    (in-generator
                     (for ([advice (advise profile-rows src)])

                       (define-values (get-src transform-collect)
                         (make-collect-transformer))

                       (define result (run (compose transform-collect
                                                    transform-taint
                                                    transform-expand
                                                    (advice-transformer advice)
                                                    transform-load)
                                           #:instant? #t
                                           #:load? #t))

                       (when (not (exception? result))
                         (yield
                          (new program%
                               [src (get-src #:clear? #t)]
                               [id (begin0 current-global-id
                                     (set! current-global-id
                                           (add1 current-global-id)))]
                               [depth (add1 depth)]
                               [sibling (begin0 sibling-cnt
                                          (set! sibling-cnt (add1 sibling-cnt)))]
                               [explanations (cons (advice-explanation advice)
                                                   (get-explanations))]))))))))

           (report 'flight:result this)]
          [(experiment-result+ main-row profile-rows [duration dur])
           ;; cosette-hack? is true
           (report 'flight:result-cosette (profiler-row-columns-incl main-row) dur)
           (set! duration #f)
           (set! children '())
           (set! state 'done)]
          [(exception+ e) (report 'flight:exception e)
                          (set! duration #f)
                          (set! children '())
                          (set! state 'done)]))

      (define/public (dump iteration)
        (when log-path
          (with-output-to-file log-path
            #:exists 'replace #:mode 'text
            (thunk
             (writeln `([fix-size ,(length (get-explanations))]
                        [iteration ,iteration])))))

        (run transform-load
             #:dump
             (λ (compiler e)
               (define path (syntax->string-path e))
               (define output-path (~a path "." algorithm))
               (define output-path-readable (~a path "." algorithm "-diff"))
               (define-values (proceed? orig-stx)
                 (match (assoc path (get-src-regular))
                   [#f (values #f #f)]
                   [(list _ orig-stx _)
                    (values (not (equal? (syntax->datum orig-stx) (syntax->datum e)))
                            orig-stx)]))

               (cond
                 [proceed? (printf "DUMPING ~a\n" path)
                           (define compiled (compiler e #f))
                           (with-output-to-file output-path
                             #:exists 'replace #:mode 'text
                             (thunk (display compiled)))
                           (define diff
                             (with-output-to-string
                               (thunk (print-diff (stx->string orig-stx) (stx->string e)))))
                           (with-output-to-file output-path-readable
                             #:exists 'replace #:mode 'text
                             (thunk (display diff)))]
                 [else (printf "SKIP DUMPING ~a\n" path)
                       (when (file-exists? output-path)
                         (printf "CLEAR DUMPING ~a\n" output-path)
                         (delete-file output-path)
                         (delete-file output-path-readable))]))))

      ;; Status: DONE
      (define/public (custom-print out . _)
        (fprintf out "<program:\n~a>"
                 (format-fields
                  `([id ,id]
                    [state ,(get-state)]
                    ,@(match (get-state)
                        ['fresh '()]
                        [_ `([metrics ,(get-metrics)])])))))

      (define/public (custom-write out)
        (custom-print out))

      (define/public (custom-display out)
        (custom-print out))))

  (define-values (get-src-deforest transform-collect-deforest)
    (make-collect-transformer))

  (define-values (get-src-regular transform-collect-regular)
    (make-collect-transformer))

  (report 'initialize:init)

  (current-deforest #f)

  (define ((transform-print x) stx phase)
    (debug: stx #:msg x #:short? #t)
    (values stx phase))

  (when (member 'deforestation pre-advisers)
    (run (compose transform-collect-deforest
                  transform-taint
                  transform-expand
                  transform-normalize
                  transform-expand
                  transform-deforest
                  transform-disarm)
         #:instant? #t))

  (report 'initialize:deforest (current-deforest))

  (run (compose transform-collect-regular
                transform-taint
                transform-expand
                transform-normalize
                transform-disarm)
       #:instant? #t)

  (define prog-regular (new program%
                            [src (get-src-regular)]
                            [id 0]
                            [depth 0]
                            [sibling 0]
                            [explanations '()]))

  (define prog
    (cond
      [(current-deforest)

       (report 'test-flight:deforest)
       (define prog-deforest (new program%
                                  [src (get-src-deforest)]
                                  [id 0]
                                  [depth 0]
                                  [sibling 0]
                                  [explanations '(deforest)]))
       (send prog-deforest activate! #:diff? #f #:print-profile? #t)

       (report 'test-flight:regular)
       (send prog-regular activate! #:diff? #f #:print-profile? #t)

       (match (get-penalty prog-regular (list prog-regular prog-deforest))
         [(list 0 0)
          #:when (send prog-regular get-duration)
          prog-regular]
         [_
          #;(run (transform-diff get-src-deforest-pure #:flip? #t) #:instant? #t)
          (when (send prog-deforest get-duration)
            (send prog-deforest dump 1))
          prog-deforest])]
      [else
       (report 'test-flight:regular)
       (send prog-regular activate! #:diff? #f #:print-profile? #t)
       prog-regular]))

  (prune? prog #f) ; call for side effect to record the program
  (report 'initialize:deforest-decision (not (eq? prog prog-regular)))
  ((current-starter) prog))

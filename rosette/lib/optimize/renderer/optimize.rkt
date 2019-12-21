#lang racket

(require racket/require
         rosette/lib/util/utility/main
         (multi-in "report" ("generic.rkt" "callgraph.rkt" "solver.rkt" "terms.rkt"))
         "renderer.rkt"
         "utils.rkt"
         "metrics.rkt"
         "structs.rkt"
         "optimize-analyze.rkt"
         "../data.rkt")

(provide make-optimize-renderer)

(detailed-struct solver-call (type start finish sat) #:mutable)

; Components that will be generating messages for this profile
(define report-components
  (list make-callgraph-component
        make-solver-calls-component
        make-terms-component))

(struct report-options (symlink? interval) #:transparent)


; Make a non-streaming report renderer
(define (make-optimize-renderer source name [options (hash)])
  (define components (for/list ([c report-components]) (c options)))
  (report-renderer source name components (report-options (hash-ref options 'symlink #f)
                                                          (hash-ref options 'interval 2.0))))

;; diff-metrics :: Map<Number> -> Map<Number> -> Map<Number>
(define (diff-metrics p1 p2)
  (for/hash! ([(k v1) p1] #:when (hash-has-key? p2 k))
    (values k (- (hash-ref p2 k) v1))))

(define (excl-metrics incl children)
  (for/hash! ([(k v) incl])
    (values k (- v (for/sum ([c children]) (hash-ref (call-node-incl c) k 0))))))

(define (compute-scores root)
  (cond
    [(not root) '()]
    [else
     (define max-values (make-hash))
     (let loop ([n root])
       (for ([c metrics])
         (define current-score (hash-ref (call-node-excl n) c 0))
         (hash-update! max-values c (λ (old) (max old current-score)) 0))
       (for-each loop (call-node-children n)))

     (let loop ([n root])
       (define score
         (for/sum ([c metrics])
           (define current-score (hash-ref (call-node-excl n) c 0))
           (/ current-score (rewire 0 1 (hash-ref max-values c 1)))))
       (set-call-node-score! n score)
       (for-each loop (call-node-children n)))

     (summarize-by (all-nodes root))]))

(define (custom-report components events)
  (define zero-metrics #f)
  (define current #f)
  (define root #f)
  (define id-to-node (make-hash))
  (define solver-calls '())

  (define (record-callgraph! graph)
    (define events (hash-ref graph 'events))
    (when (and (not (empty? events)) (not zero-metrics))
      (set! zero-metrics (hash-ref (first events) 'metrics)))
    (for ([e events])
      (case (hash-ref e 'type)
        [("ENTER" "ENTER-STRUCT")

         (define e* (cond
                      [(equal? (hash-ref e 'type) "ENTER-STRUCT")
                       (match-define (hash-table ('id id)
                                                 ('struct struct-type)
                                                 ('metrics metrics)) e)
                       (hash 'id id 'function (~a "struct:" struct-type)
                             'inputs (hash 'signature '()) 'source #f 'callsite #f 'metrics metrics)]
                      [else e]))

         (match-define (hash-table ('id id)
                                   ('function function)
                                   ('inputs inputs)
                                   ('source source)
                                   ('callsite callsite)
                                   ('metrics metrics)) e*)
         (when (and (not current) root)
           (error "multiple root procedure calls"))
         (define dm (diff-metrics zero-metrics metrics))
         (define node (call-node+ id
                                  [name function]
                                  callsite
                                  source
                                  [start (hash-ref dm 'time)]
                                  [finish #f]
                                  [start-metrics dm]
                                  [finish-metrics #f]
                                  [is-finished #f]
                                  inputs
                                  [outputs (make-hash)]
                                  [children '()]
                                  [parent current]
                                  [incl (make-hash)]
                                  [excl (make-hash)]
                                  [score 0]))
         (when current
           (set-call-node-children! current
                                    (cons node (call-node-children current))))
         ; might be the first call
         (when (not root) (set! root node))
         (set! current node)
         (hash-set! id-to-node id node)]
        [("EXIT" "EXIT-STRUCT")
         (when (not current) (error "unbalanced EXIT event"))
         (define e* (cond
                      [(equal? (hash-ref e 'type) "EXIT-STRUCT")
                       (hash 'metrics (hash-ref e 'metrics)
                             'outputs (hash))]
                      [else e]))
         (match-define (hash-table ('metrics metrics)
                                   ('outputs outputs)) e*)
         (define dm (diff-metrics zero-metrics metrics))
         (set-call-node-finish! current (hash-ref dm 'time))
         (set-call-node-finish-metrics! current dm)
         (set-call-node-outputs! current outputs)
         (set-call-node-is-finished! current #t)

         (define incl (diff-metrics (call-node-start-metrics current) dm))

         (set-call-node-incl! current incl)
         (set-call-node-excl! current (excl-metrics incl (call-node-children current)))
         (set! current (call-node-parent current))]
        [else (void)]))

    (when (not (empty? events))
      (define fake-finish-metrics
        (diff-metrics zero-metrics (hash-ref (last events) 'metrics)))

      (define fake-finish-time (hash-ref fake-finish-metrics 'time))

      (define curr current)

      (let loop ()
        (when curr
          (when (not (call-node-is-finished curr))
            (set-call-node-finish! curr fake-finish-time)
            (set-call-node-finish-metrics! curr fake-finish-metrics)
            (define incl (diff-metrics (call-node-start-metrics curr)
                                       fake-finish-metrics))
            (set-call-node-incl! curr incl)
            (set-call-node-excl! curr (excl-metrics incl (call-node-children curr))))
          (set! curr (call-node-parent curr))
          (loop)))))

  (define (record-solver-calls! calls)
    (define events (hash-ref calls 'events))
    (when (not zero-metrics)
      (error "solver-calls expects profile data first"))
    (define start-time (hash-ref zero-metrics 'time))
    (for ([e events])
      (case (hash-ref e 'type)
        [("start")
         (match-define (hash-table ('part part) ('time time)) e)
         (define typ (case part
                       [("solver") 'solve]
                       [("encode") 'encode]
                       [else 'finitize]))
         (set! solver-calls (cons (solver-call typ
                                               (- time start-time)
                                               #f
                                               #f) solver-calls))]
        [("finish")
         (match solver-calls
           [(list-rest curr _)
            ; NOTE(Oak): because we cons to the front, match
            ; the first one instead
            (define ending-time (hash-ref e 'time))
            (set-solver-call-finish! curr (- ending-time start-time))
            (when (eq? 'solve (solver-call-type curr))
              (set-solver-call-sat! curr (hash-ref e 'sat)))]
           ['() #f])])))

  (define (record-unused-terms! terms)
    (define data (hash-ref terms 'data)) ; list of (call-id, #unused) pairs
    (for ([pair data])
      (match-define (list id num) pair)
      (define node (hash-ref id-to-node id #f))
      (when node
        (hash-set! (call-node-excl node) 'unused-terms num))))

  ;; we elide `source` and `name`
  (for ([c components])
    (for ([e (receive-data c events)])
      (case (hash-ref e 'type)
        [("callgraph") (record-callgraph! e)]
        [("solver-calls") (record-solver-calls! e)]
        [("unused-terms") (record-unused-terms! e)])))

  (compute-scores root))

(struct report-renderer (source name components options)
  #:transparent
  #:methods gen:renderer
  [(define (start-renderer self profile reporter)
     (for ([c (report-renderer-components self)])
       (init-component c)))
   (define (finish-renderer self profile)
     (match-define (report-renderer source name components options) self)
     (define events (reverse (unbox (profile-state-events profile))))
     (custom-report components events))])

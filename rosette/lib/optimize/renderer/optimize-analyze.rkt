#lang racket/base

(provide nodes->profiler-row all-nodes summarize-by)
(require racket/match
         racket/function
         racket/set
         racket/contract
         racket/list
         rosette/lib/util/utility/main
         "utils.rkt"
         "metrics.rkt"
         "structs.rkt")

(define (get-aggregate-key-for-node node)
  (format "~a (~a)" (call-node-name node) (call-node-callsite node)))

(define/contract (extract-pos sigs) (list? . -> . list?)
  (for/list ([i (in-naturals)]
             [sig sigs]
             #:when (or (string=? sig "union")
                        (string=? sig "struct")
                        (string=? sig "serval_hack")))
    (cons i sig)))

(define/contract (all-nodes node) (call-node? . -> . list?)
  (cond
    [node (cons node (append-map all-nodes (call-node-children node)))]
    [else '()]))

(define/contract (nodes->profiler-row nodes) (list? . -> . profiler-row?)
  (define f (first nodes))
  ;; doing the aggregation
  (define columns
    (for/list ([c metrics])
      (for/sum ([n nodes])
        (hash-ref (call-node-excl n) c 0))))
  (define columns-incl
    (for/list ([c metrics])
      (for/sum ([n nodes])
        (hash-ref (call-node-incl n) c 0))))
  (define score (apply + (map call-node-score nodes)))
  (define sigs
    (counter->list
     (counter
      (map (compose1 extract-pos
                     (curryr hash-ref 'signature)
                     call-node-inputs)
           nodes))))

  (define out-sigs
    (counter->list
     (counter
      (map (compose1 extract-pos
                     (curryr hash-ref 'signature '())
                     call-node-outputs)
           nodes))))

  (profiler-row+
   [function (call-node-name f)]
   [callsite (call-node-callsite f)]
   ;; the above two are used as the key, so extracting it from any node
   ;; is equally OK
   [parent (filter-map (λ (node)
                         (and=> node
                                => call-node-parent
                                => get-aggregate-key-for-node))
                       nodes)]
   score
   columns
   columns-incl
   [source (remove-duplicates (map call-node-source nodes))]
   sigs
   out-sigs))

(define (summarize-by nodes)
  (define (get-key row)
    (match-define (profiler-row+ function callsite) row)
    (format "~a (~a)" function callsite))

  (define profile-row-mapped*
    (for/hash ([(key nodes) (in-hash (aggregate
                                      nodes
                                      get-aggregate-key-for-node
                                      (λ (v k x) (cons x v)) '()))])
      (values key (nodes->profiler-row nodes))))

  (define dummy-row (profiler-row+ [function "<root>"]
                                   [callsite "<root>"]
                                   [parent (hash-keys profile-row-mapped*)]
                                   [score +inf.0]
                                   [columns '()]
                                   [columns-incl '()]
                                   [source ""]
                                   [sigs '()]
                                   [out-sigs '()]))

  (define profile-row-mapped
    (hash-set profile-row-mapped* "<root> (<root>)" dummy-row))

  (define seen (mutable-set))

  (define (transform-row row)
    (define key (get-key row))
    (cond
      [(set-member? seen key) #f]
      [else (set-add! seen key)
            (fill-parents! row)
            row]))

  (define (fill-parents! row)
    (define rows (sort (map (curry hash-ref profile-row-mapped)
                            (profiler-row-parent row))
                       >
                       #:key profiler-row-score))
    (set-profiler-row-parent! row (filter-map transform-row rows)))

  (fill-parents! dummy-row)
  (double (profiler-row-parent dummy-row)
          (hash-ref profile-row-mapped "the-profiled-thunk (#f)")))

#lang racket

(require racket/hash racket/struct
         "data.rkt" "record.rkt" "reporter.rkt" "feature.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile graph data structures

;; A profile node is an entry in the dynamic control flow graph of the
;; profiled code. It contains a pointer to its parent node,
;; a list of children nodes, and a profile-data struct that contains the
;; actual data for the profile.
(struct profile-node (id parent children data) #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'profile-node)
      (lambda (obj) (let ([d (profile-node-data obj)])
                      (if d
                          (list (let ([proc (profile-data-procedure d)])
                                  (if (symbol? proc) proc (object-name proc)))
                                (metrics-ref (profile-data-start d) 'time))
                          (list #f #f))))))])


;; Profile data for a single procedure invocation.
;; * The location field stores the location at which the given procedure was
;;   invoked.
;; * The procedure field is the invoked procedure
;; * The inputs and outputs fields are assoc lists from features to numbers.
;;   For each feature in enabled-features, they store the value of that
;;   feature for the inputs and outputs of the current invocation.
;; * The metrics field is a hash map from symbols to numbers, where each
;;   symbol describes a performance metric collected during symbolic evaluation,
;;   e.g., cpu time, real time, gc time, the number of merge invocations, the number
;;   of unions and terms created, etc.
;; * The start and finish fields track the value of various metrics at the entry
;;   and exit to the current invocation, respectively.
(struct profile-data (location procedure inputs outputs metrics start finish) #:mutable #:transparent)



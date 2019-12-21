#lang racket/base

(provide (all-defined-out))
(require racket/contract
         rosette/lib/util/utility/main)

(struct+ call-node (id name callsite source start finish start-metrics finish-metrics
                      is-finished inputs outputs children parent incl excl score)
  #:mutable #:prefab)

(struct+ profiler-row (function callsite score columns columns-incl source sigs parent out-sigs) #:mutable #:prefab)

(define/contract (profiler-row->short-row row) (profiler-row? . -> . list?)
  (with (profiler-row+ function callsite score columns columns-incl source sigs out-sigs) row)
  `([function ,function]
    [callsite ,callsite]
    [score ,score]
    [columns-incl ,columns-incl]
    [columns ,columns]
    [source ,source]
    [sigs ,sigs]
    [out-sigs ,out-sigs]))

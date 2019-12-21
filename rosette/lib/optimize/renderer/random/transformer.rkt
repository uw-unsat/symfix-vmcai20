#lang racket/base

(provide transform-split-all* transform-split-all)
(require (only-in rosette split-all* split-all)
         rosette/lib/util/utility/main)

(define (transform-split-all* target-id target upward-frame downward-frame orig-stx)
  (make-syntax-transformer
   (λ (stx)
     (if (eq? stx orig-stx)
         (upward-frame
          #`(split-all* (#,target-id #,target)
                        #,(downward-frame target-id)))
         stx))))

(define (transform-split-all target upward-frame downward-frame orig-stx)
  (make-syntax-transformer
   (λ (stx)
     (if (eq? stx orig-stx)
         (upward-frame #`(split-all (#,target) #,(downward-frame target)))
         stx))))

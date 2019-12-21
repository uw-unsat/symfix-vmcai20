#lang racket/base

(provide (all-defined-out))
(require rosette/lib/util/utility/main)

(struct+ advice (transformer explanation) #:transparent)

;; cross-phase

(struct+ mod-info (mod mod-pretty filename) #:prefab)
(struct+ experiment-result (profile-rows duration main-row) #:prefab)
(struct+ exception (e) #:prefab)

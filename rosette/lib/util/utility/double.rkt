#lang racket/base

(provide double double?
         (rename-out [set-double-fst! set-fst!]
                     [set-double-snd! set-snd!]
                     [double-fst fst]
                     [double-snd snd]))

(struct double (fst snd) #:transparent #:mutable)

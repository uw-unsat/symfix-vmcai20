#lang racket/base

(require racket/string
         racket/match
         syntax/id-set
         (prefix-in rosette: rosette)
         (only-in rosette/base/form/control branch-and-merge))

(provide (all-defined-out))

(define SAFE-BUILT-IN
  (immutable-free-id-set
   (list #'branch-and-merge

         #'rosette:foldr #'rosette:build
         #'rosette:map #'rosette:filter

         ;; bool.rkt
         #'rosette:boolean? #'rosette:false?
         #'rosette:|| #'rosette:&& #'rosette:!
         #'rosette:=> #'rosette:<=>
         #'rosette:forall #'rosette:exists

         ;; real.rkt
         #'rosette:integer? #'rosette:real? #'rosette:int?
         #'rosette:< #'rosette:> #'rosette:<= #'rosette:>=
         #'rosette:+ #'rosette:- #'rosette:* #'rosette:=
         ;; TODO(Oak): make sure that /, quotient, etc.
         ;; are OK to use because they modify assertion store
         #'rosette:/ #'rosette:quotient #'rosette:remainder
         #'rosette:modulo #'rosette:abs
         #'rosette:integer->real #'rosette:real->integer

         ;; numerics.rkt
         #'rosette:number? #'rosette:zero? #'rosette:positive?
         #'rosette:negative? #'rosette:even? #'rosette:odd?
         #'rosette:add1 #'rosette:sub1 #'rosette:sgn
         #'rosette:truncate #'rosette:floor #'rosette:ceiling
         #'rosette:min #'rosette:max #'rosette:exact->inexact
         #'rosette:inexact->exact #'rosette:expt

         ;; bitvector.rkt
         #'rosette:bv #'rosette:bv? #'rosette:bitvector-size
         #'rosette:bitvector? #'rosette:bqeq #'rosette:bvslt
         #'rosette:bvsgt #'rosette:bvsle #'rosette:bvsge
         #'rosette:bvult #'rosette:bvugt #'rosette:bvule
         #'rosette:bvuge #'rosette:bvnot #'rosette:bvor
         #'rosette:bvand #'rosette:bvxor #'rosette:bvshl
         #'rosette:bvshr #'rosette:bvashr #'rosette:bvneg
         #'rosette:bvadd #'rosette:bvsub #'rosette:bvsub
         #'rosette:bvmul #'rosette:bvudiv #'rosette:bvsdiv
         #'rosette:bvurem #'rosette:bvsrem #'rosette:bvumod
         #'rosette:concat #'rosette:extract
         #'rosette:sign-extend #'rosette:zero-extend
         #'rosette:integer->bitvector
         #'rosette:bitvector->integer
         #'rosette:bitvector->natural

         ;; function.rkt
         #'rosette:fv? #'rosette:~> #'rosette:function?

         ;; distinct.rkt
         #'rosette:distinct?

         ;; equality.rkt
         #'rosette:equal? #'rosette:eq?

         ;; reflect.rkt
         #'rosette:type? #'rosette:solvable?
         ;; TODO(Oak): try to include for/all
         #'rosette:term? #'rosette:constant?
         #'rosette:expression? #'rosette:term=?

         ;; box.rkt -- skip because of side effect

         ;; list.rkt
         #'rosette:pair? #'rosette:null? #'rosette:cons
         #'rosette:null #'rosette:car #'rosette:cdr
         #'rosette:list? #'rosette:list #'rosette:length
         #'rosette:list-ref #'rosette:list-tail
         #'rosette:append #'rosette:reverse

         #'rosette:andmap #'rosette:ormap #'rosette:for-each
         #'rosette:foldl
         #'rosette:remove #'rosette:remq #'rosette:remove*
         #'rosette:remq* #'rosette:sort
         #'rosette:member #'rosette:memq #'rosette:memf
         #'rosette:findf
         #'rosette:assoc #'rosette:assq #'rosette:assf

         #'rosette:caar #'rosette:cadr #'rosette:cdar
         #'rosette:cddr

         #'rosette:caaar #'rosette:caadr #'rosette:cadar
         #'rosette:caddr #'rosette:cdaar #'rosette:cdadr
         #'rosette:cddar #'rosette:cdddr

         #'rosette:caaaar #'rosette:caaadr #'rosette:caadar
         #'rosette:caaddr #'rosette:cadaar #'rosette:cadadr
         #'rosette:caddar #'rosette:cadddr #'rosette:cdaaar
         #'rosette:cdaadr #'rosette:cdadar #'rosette:cdaddr
         #'rosette:cddaar #'rosette:cddadr #'rosette:cdddar
         #'rosette:cddddr

         #'rosette:empty? #'rosette:cons?
         #'rosette:first #'rosette:rest
         #'rosette:second #'rosette:third #'rosette:forth
         #'rosette:fifth #'rosette:sixth #'rosette:seventh
         #'rosette:eighth #'rosette:ninth #'rosette:tenth
         #'rosette:last #'rosette:last-pair
         #'rosette:take #'rosette:drop #'rosette:split-at
         #'rosette:take-right #'rosette:drop-right
         #'rosette:split-at-right #'rosette:add-between
         #'rosette:append* #'rosette:flatten
         #'rosette:remove-duplicates
         #'rosette:filter-map #'rosette:count
         #'rosette:partition #'rosette:append-map
         #'rosette:filter-not
         ;; NOTE(Oak): non-deterministic, but fine to include
         #'rosette:shuffle
         #'rosette:argmax #'rosette:argmin
         #'rosette:insert #'rosette:replace


         ;; vector.rkt
         #'rosette:vector? #'rosette:vector
         #'rosette:vector-immutable
         #'rosette:vector-length #'rosette:vector-ref
         ;; ignore functions with side-effects
         #'rosette:vector->list #'rosette:list->vector
         #'rosette:vector->immutable-vector
         #'rosette:vector-append

         ;; procedure.rkt
         #'rosette:procedure? #'rosette:apply
         #'rosette:negate #'rosettte:void?

         ;; control.rkt
         #'rosette:not #'rosette:xor

         ;; safe unlifted functions
         #'rosette:empty
         #'rosette:build-list
         #'rosette:build-vector
         #'rosette:string?
         #'rosette:values
         #'rosette:void)))

(define (expanding-path? p) (string-suffix? (path->string p) "expanded module.rkt"))

(define (new-module-level stx current-module-info)
  (define source (syntax-source stx))
  (match (current-module-info)
    [(list path mod ...)
     (define name (syntax->datum stx))
     #;(printf "old-path: ~a, new-path: ~a, mods: ~a\n" path source `(,@mod ,name))
     ;; NOTE(Oak): it's unclear what path we should use. Use the old one for now
     `(,path ,@mod ,name)]
    [else (list source)]))

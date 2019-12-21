#lang racket/base

(provide advise-random)

(require racket/generator
         racket/list
         racket/match
         racket/function
         racket/syntax
         syntax/parse
         syntax/stx
         syntax/id-set
         (only-in rosette/base/form/control
                  [branch-and-merge rosette:branch-and-merge])
         rosette/lib/util/utility/main
         rosette/lib/util/utility/syntax/free-vars

         "../../struct.rkt"

         "../../../syntax-utils/utils.rkt"
         "../../optimize-reporter.rkt"
         "../utils.rkt"
         "../optimize-analyze.rkt"
         "../structs.rkt"
         "../../../optimization.rkt"
         "./transformer.rkt"

         syntax/parse/define
         (for-syntax racket/base))

(struct+ advice-random (forall target type) #:transparent)

(define-simple-macro (shuffle-expr e ...)
  (for-each (λ (f) (f)) (shuffle (list (thunk e) ...))))

(define (report msg . payload) (void))

;; find the value to quantify on the expanded code and all frames
(define (find-all-target+frames stx phase)

  (define trans-module
    (make-module-transformer
     #:custom
     (λ (body name) (map (λ (e) (cons e (trans-top e 0))) (shuffle (syntax->list body))))))


  (define (trans-expr stx)
    (define orig-stx stx)
    (define (trans-expr stx pair-frames parent-frame)
      (define-simple-macro (traverse #:frame subst:id {~literal =>} frame-expr:expr
                                     #:skip-forall? skip-forall?:boolean
                                     #:skip-yield? skip-yield?:boolean
                                     #:tag tag:expr
                                     #:target target-expr:expr)
        (let ()
          (define ((thin-frame wrapper) subst)
            (wrapper (syntax/prop this-syntax frame-expr)))
          (traverse/core thin-frame target-expr skip-forall? skip-yield? tag)))

      (define (traverse/core thin-frame target skip-forall? skip-yield? tag)

        (let ([pair-frames (map (λ (pair-frame) (list (thin-frame (first pair-frame))
                                                      (second pair-frame)
                                                      (cons (syntax->datum tag) (third pair-frame))))
                                (if skip-forall?
                                    pair-frames
                                    (cons (list identity parent-frame '()) pair-frames)))]
              [parent-frame (thin-frame parent-frame)])
          (when (and (not skip-yield?)
                     (syntax-parse target
                       #:literal-sets (kernel-literals)
                       [(_:leaf-form . _) #f]
                       [({~or case-lambda #%plain-lambda set!} . _) #f]
                       [_ #t]))
            (yield (list pair-frames target orig-stx)))
          (trans-expr target pair-frames parent-frame)))

      (syntax-parse stx
        #:literal-sets (kernel-literals)
        [(typ:define-values vars rhs)
         ;; define-values is at the top-level, so it's not for/all-able
         (traverse
          #:frame subst => #`(typ vars #,subst)
          #:skip-forall? #t
          #:skip-yield? #t
          #:tag #'typ
          #:target #'rhs)]

        ;; Atomic
        [_:id #f]
        [(_:leaf-form ~! . _) #f]

        ;; Compound
        [(typ:#%plain-lambda fmls body)
         (traverse
          #:frame subst => #`(typ fmls #,subst)
          #:skip-forall? #t
          #:skip-yield? #f
          #:tag #'typ
          #:target #'body)]

        [(typ:case-lambda [f-fmls f-body] ...)
         (for-each
          (λ (f) (f))
          (shuffle (for/list ([fmls (in-list (attribute f-fmls))]
                              [body (in-list (attribute f-body))]
                              [pos (in-naturals)])
                     (thunk (traverse
                             #:frame subst => #`(typ
                                                 .
                                                 #,(list-set
                                                    (map list (attribute f-fmls) (attribute f-body))
                                                    pos
                                                    (list fmls subst)))
                             #:skip-forall? #t
                             #:skip-yield? #f
                             #:tag #'typ
                             #:target body)))))]

        [(typ:set! x rhs)
         ;; (for/all on e (set! x C[e])) is completely equivalent to (set! e (for/all on e C[e]))
         ;; so let's skip it
         (traverse
          #:frame subst => #`(typ x #,subst)
          #:skip-forall? #t
          #:skip-yield? #f
          #:tag #'typ
          #:target #'rhs)]

        [(typ:let-values () e1 e2)
         (traverse
          #:frame subst => #`(typ () #,subst e2)
          #:skip-forall? #f
          #:skip-yield? #f
          #:tag #'typ
          #:target #'e1)
         (traverse
          #:frame subst => #`(typ () e1 #,subst)
          #:skip-forall? #f
          #:skip-yield? #f
          #:tag #'typ
          #:target #'e2)]

        [(typ:let-values-form ([form.ids form.rhs] ...) form.body)
         (for-each
          (λ (f) (f))
          (shuffle
           (cons
            (thunk
             (traverse
              #:frame subst => #`(typ ([form.ids form.rhs] ...) #,subst)
              #:skip-forall? #f
              #:skip-yield? #f
              #:tag #'typ
              #:target #'form.body))
            (for/list ([ids (attribute form.ids)]
                       [rhs (attribute form.rhs)]
                       [pos (in-naturals)])
              (thunk
               (traverse
                #:frame subst => #`(typ
                                    #,(list-set (map list
                                                     (attribute form.ids)
                                                     (attribute form.rhs))
                                                pos
                                                (list ids subst))
                                    form.body)
                #:skip-forall? #f
                #:skip-yield? #f
                #:tag #'typ
                #:target rhs))))))]

        [({~and typ {~or if begin begin0 #%plain-app}} args ...)
         (for-each
          (λ (f) (f))
          (shuffle
           (for/list ([arg (in-list (attribute args))] [pos (in-naturals)])
             (thunk
              (traverse
               #:frame subst => #`(typ . #,(list-set (attribute args) pos subst))
               #:skip-forall? #f
               #:skip-yield? #f
               #:tag #'typ
               #:target arg)))))]

        [(typ:with-continuation-mark key val result)
         ;; with-continuation-mark is weird. Let's not for/all'ing on it
         (traverse
          #:frame subst => #`(typ key val #,subst)
          #:skip-forall? #t
          #:skip-yield? #t
          #:tag #'typ
          #:target #'result)]

        [_ (error 'transform "unrecognized expression form: ~.s"
                  (syntax->datum stx))])
      #'#f)

    (trans-expr stx '() identity))

  (define trans-top (make-top-transformer trans-expr trans-module))

  (define (transform-find-uexpr stx phase) (values (trans-top stx phase) phase))

  (in-generator (transform-find-uexpr stx phase)))

(define (no-side-effect? stx) (safe? stx))

(define (acceptable? tags)
  (not (or (member '#%plain-lambda tags)
           (member 'lambda tags)
           (member 'case-lambda tags)
           (member 'if tags))))

(define (advise-random _ src)
  (in-generator
    (for ([item (in-list (shuffle src))])
      (match-define (list path stx phase) item)

      (for ([result (find-all-target+frames stx phase)])
        (match-define (list pair-frames target orig) result)
        (for ([pair-frame (in-list pair-frames)])
          (match-define (list downward-frame upward-frame downward-tags) pair-frame)

          (define (generate target-id)
            ;; if target-id is #f, then only do forall on mutable struct
            (cond
              [target-id (shuffle-expr
                          (yield
                           (advice+
                            [explanation (advice-random original-forall-expr target "union")]
                            [transformer (transform-split-all*
                                          target-id
                                          target
                                          upward-frame
                                          downward-frame
                                          orig)]))
                          (generate #f))]
              [else (yield
                     (advice+
                      [explanation (advice-random original-forall-expr target "struct")]
                      [transformer (transform-split-all
                                    target
                                    upward-frame
                                    downward-frame
                                    orig)]))]))

          (define original-forall-expr (downward-frame target))
          (define required-bound-ids (free-vars target))
          (define bound-in-frame (free-vars original-forall-expr))

          (when (andmap (λ (id) (member id bound-in-frame free-identifier=?))
                        required-bound-ids)
            (cond
              [(and (identifier? target) (no-set!? target original-forall-expr))
               (generate target)]
              [(and (not (identifier? target))
                    (no-side-effect? original-forall-expr)
                    (acceptable? downward-tags))
               (generate (generate-temporary 'target))]
              [(and (not (identifier? target))
                    (no-side-effect? target)
                    (acceptable? downward-tags))
               (generate #f)])))))))

#lang racket

(provide advise-path-split)

(require syntax/parse
         syntax/stx
         syntax/id-set
         racket/generator
         racket/function
         (only-in rosette/base/form/control
                  [branch-and-merge rosette:branch-and-merge])
         rosette/lib/util/utility/main
         rosette/lib/util/utility/syntax/free-vars

         "../../struct.rkt"
         "structs.rkt"

         "../../../syntax-utils/utils.rkt"
         "../../optimize-reporter.rkt"
         "../utils.rkt"
         "./transformer.rkt"
         "../optimize-analyze.rkt"
         "../structs.rkt")

(define (report msg . payload) (void))

(define (find-forall-expr stx target-expr frame)
  (define occurrences (count (curry free-identifier=? target-expr)
                             (free-vars frame #:module-bound? #t)))
  (cond
    [(positive? occurrences)
     ;; target-expr is bound
     ;; `(for/all ([x x]) body)`: equivalent to `body` when `x` is not `set!`
     (define safe? (syntax-parse frame
                     #:literal-sets (kernel-literals)
                     [(define-values _ _)
                      (report 'path-split:advise:forall:frame-define frame)
                      #f]
                     [_ #:when (no-set!? target-expr frame)
                        #t]
                     [_ (report 'path-split:advise:forall:frame-has-set! frame)
                        #f]))
     (and safe? (list frame occurrences))]
    [else (report 'path-split:advise:forall:out-of-scope target-expr frame)
          #f]))

;; find the value to quantify on the expanded code and all frames
(define (find-target+frames stx module-source line ch pos-sym phase)
  (define (transform-find-uexpr return)
    (make-syntax-transformer
     (let ()
       (define (trans-expr stx frames)
         (define (trans-exprs stxs) (for ([e (in-syntax stxs)]) (trans-expr e (cons e frames))))
         (syntax-parse stx
           #:literal-sets (kernel-literals)
           [(#%plain-app _ . args)
            #:when (and (equal? (syntax-source stx) module-source)
                        (syntax-line stx) (syntax-column stx)
                        (= line (syntax-line stx)) (= ch (syntax-column stx)))
            (return (list (list-ref (stx->list #'args) pos-sym) frames))]
           [(define-values _ rhs) (trans-exprs #'(rhs))]
           [_:id stx]
           [(#%plain-lambda . form:lambda-body-form)
            (trans-exprs #'(form.body ...))]
           [(case-lambda form:lambda-body-form ...)
            (trans-exprs #'({~@ form.body ...} ...))]
           [(set! _ rhs) (trans-exprs #'(rhs))]
           [(_:leaf-form ~! . _) stx]
           [(_:let-values-form . form:let-values-body-form)
            (trans-exprs #'(form.rhs ... form.body ...))]
           [(_:compound-form ~! . body) (trans-exprs #'body)]
           [_ (error 'transform "unrecognized expression form: ~.s"
                     (syntax->datum stx))])
         #'#f)
       (λ (stx) (trans-expr stx (list stx))))))
  (let/ec return
    ((transform-find-uexpr return) stx phase)
    #f))

(define (process-row the-row src)
  (match the-row
    [(profiler-row+ [function f]
                    [callsite (and callsite
                                   (pregexp #px"(.*?):(\\d+):(\\d+)"
                                            (list _
                                                  path
                                                  (app string->number line)
                                                  (app string->number ch))))]
                    [sigs `((,sym-loc-list . ,_) ...)]
                    parent
                    columns)
     (define sym-locs (remove-duplicates (append* sym-loc-list)))
     (report 'path-split:advise:row f callsite columns sym-locs)
     (define current
       (match (assoc path src)
         [(list _ expanded-stx phase)
          (in-spread
           (in-generator
            (define module-source (syntax-source expanded-stx))

            (for* ([sym-loc sym-locs])
              (match-define (cons pos-sym type-sym) sym-loc)
              (match type-sym
                [(or "union" "struct")
                 (match (find-target+frames expanded-stx
                                            module-source
                                            line ch
                                            pos-sym phase)
                   [#f (report 'path-split:advise:value-not-found module-source line ch)]
                   [(list target forall-frames)
                    (report 'path-split:advise:target target)
                    (define filtered-frames
                      (reverse
                       (for/list ([forall-frame (in-list forall-frames)])
                         (when (not (identifier? target))
                           (report 'path-split:advise:forall:bad-target
                                   target module-source line ch))
                         #:break (not (identifier? target))
                         (define item (find-forall-expr expanded-stx
                                                        target
                                                        forall-frame))
                         #:break (not item)
                         item)))

                    (cond
                      [(empty? filtered-frames)
                       (debug: forall-frames)]
                      [else
                       (define first-frame-item (first filtered-frames))
                       (define optimal-frame-item (last (takef filtered-frames (λ (item) (= (second item) (second first-frame-item))))))
                       (define last-frame-item (last filtered-frames))

                       (define candidates (remove-duplicates (list first-frame-item
                                                                   optimal-frame-item
                                                                   last-frame-item)
                                                             eq?))

                       (yield
                        (for/list ([frame-item (in-list candidates)])
                          (match-define (list forall _) frame-item)
                          (advice-path-split+ forall target [type type-sym])))])])]
                [_ (void)]))))]
         [_ (report 'path-split:advise:path-not-found path (map first src))
            '()]))
     (in-spread
      (list current
            (in-generator
             (for ([row (in-list parent)])
               (yield-from (process-row row src))))))]
    [(profiler-row+ [function f]
                    callsite
                    [sigs `((,sym-loc-list . ,_) ...)]
                    parent
                    columns)
     (define sym-locs (remove-duplicates (append* sym-loc-list)))
     (report 'path-split:advise:bad-callsite f callsite columns sym-locs)
     (in-generator
      (for ([row (in-list parent)])
        (yield-from (process-row row src))))]
    [_
     (report 'path-split:advise:no-parent the-row)
     '()]))

(define (advise-path-split rows src)
  (define-values (member? record!) (make-file-set ".symfix"))

  (in-zigzag
   (in-generator
    (for ([row (in-list rows)])
      (yield
       (in-generator
        (for ([adv (process-row row src)])
          (match-define (advice-path-split+ forall target type) adv)
          (define adv-repr (list (syntax->datum forall)
                                 (syntax->datum target)
                                 type))
          (cond
            [(member? adv-repr) (report 'path-split:advise:elide adv-repr)]
            [else (record! adv-repr)
                  (yield (advice+
                          [transformer (transform-path-split forall target type)]
                          [explanation adv]))]))))))))

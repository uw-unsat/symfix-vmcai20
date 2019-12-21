#lang racket

(provide advise-serval-hack)

(require syntax/parse
         syntax/stx
         syntax/id-set
         racket/generator
         racket/function
         (only-in rosette/base/form/control
                  [branch-and-merge rosette:branch-and-merge])
         rosette/lib/util/utility/main

         "../../struct.rkt"
         "structs.rkt"

         "../../../syntax-utils/utils.rkt"
         "../../optimize-reporter.rkt"
         "../utils.rkt"
         "./transformer.rkt"
         "../optimize-analyze.rkt"
         "../structs.rkt")

(define (report msg . payload) (void))

;; find the value to quantify on the expanded code and all frames
(define (find-target stx module-source line ch phase)
  (define (transform-find-uexpr return)
    (make-syntax-transformer
     (let ()
       (define (trans-expr stx)
         (define (trans-exprs stxs) (for ([e (in-syntax stxs)]) (trans-expr e)))
         (syntax-parse stx
           #:literal-sets (kernel-literals)
           [(#%plain-app _ . args)
            #:when (and (equal? (syntax-source stx) module-source)
                        (syntax-line stx) (syntax-column stx)
                        (= line (syntax-line stx)) (= ch (syntax-column stx)))
            (return this-syntax)]
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
       trans-expr)))
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
                    [out-sigs `((,sym-loc-list . ,_) ...)]
                    parent
                    columns)
     (define sym-locs (remove-duplicates (append* sym-loc-list)))
     (report 'serval:advise:row f callsite columns sym-locs)
     (define current
       (match (assoc path src)
         [(list _ expanded-stx phase)
          (in-generator
           (define module-source (syntax-source expanded-stx))

           (for* ([sym-loc sym-locs])
             (match-define (cons pos-sym type-sym) sym-loc)
             (match type-sym
               [(or "serval_hack")
                (match (find-target expanded-stx
                                    module-source
                                    line ch phase)
                  [#f (report 'serval:advise:value-not-found module-source line ch)]
                  [target
                   (report 'serval:advise:target target)
                   (yield (advice-serval-hack+ target))])]
               [_ (void)])))]
         [_ (report 'serval:advise:path-not-found path (map first src))
            '()]))
     (in-spread
      (list current
            (in-generator
             (for ([row (in-list parent)])
               (yield-from (process-row row src))))))]
    [(profiler-row+ [function f]
                    callsite
                    [out-sigs `((,sym-loc-list . ,_) ...)]
                    parent
                    columns)
     (define sym-locs (remove-duplicates (append* sym-loc-list)))
     (report 'serval:advise:bad-callsite f callsite columns sym-locs)
     (in-generator
      (for ([row (in-list parent)])
        (yield-from (process-row row src))))]
    [_
     (report 'serval:advise:no-parent the-row)
     '()]))

(define (advise-serval-hack rows src)
  (define-values (member? record!) (make-file-set ".symfix"))

  (in-zigzag
   (in-generator
    (for ([row (in-list rows)])
      (yield
       (in-generator
        (for ([adv (process-row row src)])
          (match-define (advice-serval-hack+ target) adv)
          (define adv-repr (syntax->datum target))
          (cond
            [(member? adv-repr) (report 'serval:advise:elide adv-repr)]
            [else (record! adv-repr)
                  (yield (advice+
                          [transformer (transform-serval-hack target)]
                          [explanation adv]))]))))))))

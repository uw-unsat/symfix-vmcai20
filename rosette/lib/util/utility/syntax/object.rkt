#lang racket/base

(require racket/function
         racket/list
         syntax/parse
         "../debug.rkt"
         loop)

(provide (all-defined-out)
         (all-from-out 'with-contracts))

(module with-contracts racket/base
  (require racket/pretty
           racket/contract
           racket/list
           racket/function
           racket/match
           racket/format
           syntax/parse
           "../debug.rkt")

  (define module-path/c (or/c 'kernel (cons/c path-string? (listof symbol?))))

  (provide (contract-out
            [occurs-in? (-> syntax? identifier? boolean?)]
            [path-string->string (-> (or/c string? path?) string?)]
            [stx->string (-> syntax? string?)]
            [stx-flatten-id (-> syntax? (listof identifier?))]
            [free-id-in? (-> identifier? syntax? any/c)]
            [current-module-path (parameter/c any/c)]
            [identifier-binding*
             (-> identifier? (or/c #f (listof (cons/c symbol? module-path/c))))]
            [mpi->path (-> module-path-index? module-path/c)]
            [syntax->string-path (-> syntax? string?)]
            [keep-lambda-properties (-> syntax? syntax? syntax?)]
            [length-improper (-> any/c number?)]
            [make-adjust-loc (-> exact-nonnegative-integer? (-> syntax? syntax?))]
            [syntax/prop (-> syntax? syntax? syntax?)]
            [original-base-line number?]))

  ;; measure the length of an improper list
  ;; where the last pair counts as one
  (define (length-improper xs)
    (cond
      [(cons? xs) (add1 (length-improper (cdr xs)))]
      [else 0]))

  (define (keep-lambda-properties orig new)
    (let ([p (syntax-property orig 'method-arity-error)]
          [p2 (syntax-property orig 'inferred-name)])
      (let ([new (if p
                     (syntax-property new 'method-arity-error p)
                     new)])
        (if p2
            (syntax-property new 'inferred-name p2)
            new))))

  (define (syntax->string-path stx) (path-string->string (syntax-source stx)))

  (define (occurs-in? stx sym)
    (syntax-parse stx
      [_:id (free-identifier=? stx sym)]
      [(a . b) (or (occurs-in? #'a sym) (occurs-in? #'b sym))]
      [_ #f]))

  (define (path-string->string p)
    (cond
      [(path? p) (path->string p)]
      [else p]))

  (define (stx->string stx) (pretty-format (syntax->datum stx)))

  (define stx-flatten-id
    (syntax-parser
      [id:id (list #'id)]
      [(a . b) (append (stx-flatten-id #'a) (stx-flatten-id #'b))]
      [_ '()]))

  (define (free-id-in? x xs)
    (memf ((curry free-identifier=? x) (stx-flatten-id xs))))


  (define current-module-path (make-parameter #f))

  ;; BEGIN SECTION from
  ;; https://github.com/greghendershott/racket-mode/blob/master/racket/find.rkt

  ;; Distill identifier-binding to what we need. Unfortunately it can't
  ;; report the definition id in the case of a contract-out and a
  ;; rename-out, both. For `(provide (contract-out [rename orig new
  ;; contract]))` it reports (1) the contract-wrapper as the id, and (2)
  ;; `new` as the nominal-id -- but NOT (3) `orig`. Instead the caller
  ;; will need try using `renaming-provide`.
  (define (identifier-binding* id)
    (define sym->id namespace-symbol->identifier)
    (match (identifier-binding id)
      [(list source-mpi         source-id
             nominal-source-mpi nominal-source-id
             source-phase import-phase nominal-export-phase)
       (list (cons source-id (mpi->path source-mpi))
             (cons nominal-source-id (mpi->path nominal-source-mpi)))]
      [_ #f]))

  (define (mpi->path mpi)
    (define (hash-bang-symbol? v)
      (and (symbol? v) (regexp-match? #px"^#%" (symbol->string v))))
    (match (resolved-module-path-name (module-path-index-resolve mpi))
      [(? hash-bang-symbol?) 'kernel]
      [(? path-string? path) (list path)]
      [(list '|expanded module| (? symbol? subs) ...)
       (list* (current-module-path) subs)]
      ['|expanded module| (list (current-module-path))]
      [(? symbol? sym) (list (build-path (current-load-relative-directory)
                                         (~a sym ".rkt")))]
      [(list (? path-string? path) (? symbol? subs) ...)
       (list* path subs)]))

  (define (syntax/prop orig stx)
    (datum->syntax orig (syntax-e stx) orig orig))

  (define original-base-line 1000000000)
  (define base-line original-base-line)

  (define ((make-adjust-loc i) stx)
    (datum->syntax
     stx
     (syntax-e stx)
     (list (current-module-path)
           (begin (set! base-line (add1 base-line)) base-line) i 1 0)
     stx)))

(require 'with-contracts)


(define current-replacements (make-parameter '()))

(define (rebuild expr replacements #:assoc [asso assq] #:collect? [collect? #f])
  (loop lp ([expr expr] [same-k (thunk expr)] [diff-k identity])
    (let ([a (asso expr replacements)])
      (cond
        [a (when collect?
             (current-replacements (cons (car a) (current-replacements))))
           (diff-k (cdr a))]
        [(pair? expr)
         (lp (car expr)
             (thunk
              (lp (cdr expr) same-k
                  (λ (y) (diff-k (cons (car expr) y)))))
             (λ (x)
               (lp (cdr expr)
                   (thunk (diff-k (cons x (cdr expr))))
                   (λ (y) (diff-k (cons x y))))))]
        [(vector? expr)
         (lp (vector->list expr) same-k
             (λ (x) (diff-k (list->vector x))))]
        [(box? expr)
         (lp (unbox expr) same-k (λ (x) (diff-k (box x))))]
        [(syntax? expr)
         (if (identifier? expr)
             (same-k)
             (lp (syntax-e expr) same-k
                 (λ (x) (diff-k (datum->syntax expr x expr expr)))))]
        [else (same-k)]))))

(define (make-compiler transformer
                       #:base [base-compiler (current-compile)])
  (λ (e immediate-eval?)
    (define stx
      (cond
        [(syntax? e) e]
        [else (namespace-syntax-introduce
               (datum->syntax #f e))]))
    (base-compiler (transformer stx) immediate-eval?)))

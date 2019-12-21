#lang racket

;; (provide force-load)
;; (require custom-load
;;          syntax/modresolve)

;; (define (path->regexp s) (regexp (regexp-quote (path->string s))))
;; (define (default-loader f) (f))
;; (define (force-load mod
;;                     #:compiler [compiler (current-compile)]
;;                     #:loader [loader default-loader]
;;                     #:target [target #f])
;;   (parameterize ([current-namespace (make-base-namespace)]
;;                  [current-load/use-compiled
;;                   (make-custom-load/use-compiled
;;                    #:blacklist
;;                    (let ([p (resolved-module-path-name
;;                              ((current-module-name-resolver)
;;                               (resolve-module-path mod)
;;                               #f
;;                               #f
;;                               #f))])
;;                      (cond
;;                       [(path? p) (path->regexp p)]
;;                       [(pair? p)
;;                        (if (path? (first p))
;;                            (path->regexp (first p))
;;                            empty)]
;;                       [else empty])))]
;;                  [current-compile compiler])
;;     (loader (thunk (dynamic-require mod target)))))

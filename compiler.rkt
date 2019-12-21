#lang racket/base

(require racket/cmdline
         rosette/lib/deforest/compiler
         rosette/lib/syntax-utils/utils
         rosette/lib/util/utility/main)

(define file-to-compile
  (command-line
   #:program "compiler"
   #:args (filename)
   filename))

(current-compile (make-rosette-compiler (compose transform-deforest transform-disarm)))
(dynamic-require file-to-compile #f)

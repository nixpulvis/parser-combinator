#lang racket/base

; Helpers shared by the parser-combinator documentation.

(require scribble/example
         (only-in racket/sandbox call-in-sandbox-context))

(provide (all-from-out scribble/example)
         parse-eval
         quick-eval
         json-eval)

; Example lines, including the "> " prompt, fit in this many columns, so
; that they fit on a phone screen. Example sources are wrapped to match.
(define example-columns 58)

; Module-Path ... -> Evaluator
; makes a sandboxed evaluator with racket/base and the given modules, which
; wraps printed results at example-columns
(define (make-doc-eval . mods)
  (define ev (make-base-eval))
  (call-in-sandbox-context
   ev
   (lambda ()
     ((dynamic-require 'racket/pretty 'pretty-print-columns) example-columns)))
  (ev `(require ,@mods))
  ev)

(define parse-eval (make-doc-eval 'parser-combinator/parse))
(define quick-eval (make-doc-eval))
(define json-eval (make-doc-eval 'parser-combinator/parse 'parser-combinator/json))

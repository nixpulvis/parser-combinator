#lang racket
(require "parse.rkt"
         "lex.rkt"
         (only-in "json.rkt" NUMBER STRING))

(provide string->json
         json-lexer
         VALUE
         ARRAY
         OBJECT
         PAIR)

; -----------------------------------
; ----- JSON over a Token stream -----
; -----------------------------------

; The same JSON as json.rkt's, parsed in two stages: `json-lexer` turns
; the text into Tokens, and the grammar below parses the Tokens. Only
; the lexer knows about whitespace, so the grammar never mentions it.

; String -> JSON
(define (string->json str)
  (parse VALUE (json-lexer str)))

; Lexer
; NUMBER and STRING are json.rkt's parsers, so a number's Token holds
; the Number, and a string's the decoded String.
(define json-lexer
  (lexer #:skip " \t\n\r"
         [begin-object (lit "{")]
         [end-object (lit "}")]
         [begin-array (lit "[")]
         [end-array (lit "]")]
         [comma (lit ",")]
         [colon (lit ":")]
         [null (pmap (const 'null) (lit "null"))]
         [true (pmap (const #t) (lit "true"))]
         [false (pmap (const #f) (lit "false"))]
         [number NUMBER]
         [string STRING]))

; Parser
(define-parser VALUE
  (alt (tok 'null)
       (tok 'true)
       (tok 'false)
       (tok 'number)
       (tok 'string)
       ARRAY
       OBJECT))

; Parser
(define-parser ARRAY
  (unwrap (tok 'begin-array)
          (sep-by* VALUE (tok 'comma))
          (tok 'end-array)))

; Parser
(define-parser OBJECT
  (pmap (lambda (open pairs close)
          (for/hasheq ([pair (in-list pairs)])
            (values (car pair) (cdr pair))))
        (tok 'begin-object)
        (sep-by* PAIR (tok 'comma))
        (tok 'end-object)))

; Parser
(define-parser PAIR
  (pmap (lambda (name colon value)
          (cons (string->symbol name) value))
        (tok 'string)
        (tok 'colon)
        VALUE))

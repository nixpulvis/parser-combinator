#lang racket
(require "parse.rkt")

(provide (struct-out json-pair)
         string->json
         ; TODO: Remove these provides.
         parse-json-string
         parse-json-number
         parse-json-pair
         parse-json-array
         parse-json-object
         parse-json-value
         STRING
         NUMBER
         ARRAY
         PAIR
         OBJECT
         VALUE)

; -------------------------------------
; ----- JSON Context Free Grammar -----
; -------------------------------------

; Parser
(define-parser SPACE
  (alt*
    (lit " ") (lit "\t") (lit "\n") (lit "\r")))

; Parser -> Parser
(define (spacey l)
  (unwrap (star SPACE) l (star SPACE)))

; Parser
(define-parser HEXIT
  (alt* DIGIT (lit "a") (lit "b") (lit "c") (lit "d") (lit "e") (lit "f")
               (lit "A") (lit "B") (lit "C") (lit "D") (lit "E") (lit "F")))

; Parser
(define-parser STRING
  (wrap 'string
         (lit "\"")
         (star (alt* (char-except "\\" "\"")
                     (lit "\\\"")
                     (lit "\\\\")
                     (lit "\\/")
                     (lit "\\b")
                     (lit "\\f")
                     (lit "\\n")
                     (lit "\\r")
                     (lit "\\t")
                     (seq* 'hex (lit "\\u") HEXIT HEXIT HEXIT HEXIT)))
         (lit "\"")))

; Parser
(define-parser DIGIT+
  (alt* (lit "1") (lit "2") (lit "3") (lit "4") (lit "5") (lit "6") (lit "7")
         (lit "8") (lit "9")))

; Parser
(define-parser DIGIT
  (alt* (lit "0") DIGIT+))

; Parser
(define-parser NUMBER
  (seq* 'number
         (opt (lit "-"))
         (alt* (lit "0") (seq* 'significand
                               DIGIT+
                               (star DIGIT)))
         (opt (seq* 'decimal (lit ".") (star DIGIT)))
         (opt (seq* 'exponent (alt* (lit "e") (lit "E"))
                    (opt (alt* (lit "+") (lit "-")))
                    (star DIGIT)))))

; Parser
(define-parser PAIR
  (seq* 'pair STRING (spacey (lit ":")) VALUE))

; Parser
(define-parser OBJECT
  (wrap 'object
         (spacey (lit "{"))
         (opt (seq* 'object-first
                    PAIR
                    (star (seq* 'object-rest
                                (spacey (lit ","))
                                PAIR))))
         (spacey (lit "}"))))

; Parser
(define-parser ARRAY
  (wrap 'array
         (spacey (lit "["))
         (opt (seq* 'array-first
                    VALUE
                    (star (seq* 'array-rest
                                (spacey (lit ","))
                                VALUE))))
         (spacey (lit "]"))))

; Parser
(define-parser VALUE
  (alt* (spacey STRING)
         (spacey NUMBER)
         (spacey OBJECT)
         (spacey ARRAY)
         (spacey (lit "true"))
         (spacey (lit "false"))
         (spacey (lit "null"))))

; ------------------------
; ----- JSON Parsing -----
; ------------------------

; A JSON is one of:
; - String
; - Number
; - Boolean
; - 'null
; - JSONArray
; - JSONObject

; A JSONArray is a [Listof JSON], representing an ordered
; collection of values.

; A JSONObject is a [Listof JSONPair], representing a set
; of named values, similar to a structure.

; A JSONPair is a (make-pair String JSON),
; representing a named value.
(define-struct json-pair (name value) #:transparent)

; (wrap-node 'string AST) -> JSONString
(define (parse-json-string ast)
  (flatten-ast ast))

; (seq-node 'number AST AST) -> JSONNumber
(define (parse-json-number ast)
  (string->number (string-append "#e" (flatten-ast ast))))

; (seq-node 'json-pair AST) -> JSONPair
(define (parse-json-pair ast)
  (make-json-pair (parse-json-string (seq-node-left ast))
        (parse-json-value (seq-node-right (seq-node-right ast)))))

; (wrap-node 'array AST) -> JSONArray
(define (parse-json-array ast)
  (let* ([v (wrap-node-value ast)]
         [f (lambda (a b) (cons (parse-json-value (seq-node-right a)) b))])
    (cond [(emp-node? v)
           empty]
          [(and (seq-node? v)
                (symbol=? (seq-node-label v) 'array-first)
                (emp-node? (seq-node-right v)))
           (list (parse-json-value (seq-node-left v)))]
          [(and (seq-node? v)
                (symbol=? (seq-node-label v) 'array-first)
                (seq-node? (seq-node-right v)))
           (cons (parse-json-value (seq-node-left v))
                 (star-foldr f empty (seq-node-right v)))]
          [else
           (error 'parse-json-array "bad input ~s" ast)])))

; (wrap-node 'object AST) -> JSONObject
(define (parse-json-object ast)
  (let* ([v (wrap-node-value ast)]
         [f (lambda (a b) (cons (parse-json-pair (seq-node-right a)) b))])
    (cond [(emp-node? v)
           empty]
          [(and (seq-node? v)
                (symbol=? (seq-node-label v) 'object-first)
                (emp-node? (seq-node-right v)))
           (list (parse-json-pair (seq-node-left v)))]
          [(and (seq-node? v)
                (symbol=? (seq-node-label v) 'object-first)
                (seq-node? (seq-node-right v)))
           (cons (parse-json-pair (seq-node-left v))
                 (star-foldr f empty (seq-node-right v)))]
          [else
           (error 'parse-json-object "bad input ~s" ast)])))

; AST -> JSON
(define (parse-json-value ast)
  (cond [(and (wrap-node? ast)
              (symbol=? (wrap-node-label ast) 'string))
         (parse-json-string ast)]
        [(and (seq-node? ast)
              (symbol=? (seq-node-label ast) 'number))
         (parse-json-number ast)]
        [(and (wrap-node? ast)
              (symbol=? (wrap-node-label ast) 'array))
         (parse-json-array ast)]
         [(and (wrap-node? ast)
              (symbol=? (wrap-node-label ast) 'object))
         (parse-json-object ast)]
        [(and (lit-node? ast)
              (string=? (lit-node-value ast) "true"))
         true]
        [(and (lit-node? ast)
              (string=? (lit-node-value ast) "false"))
         false]
        [(and (lit-node? ast)
              (string=? (lit-node-value ast) "null"))
         'null]
        [else
         (error 'parse-json-value "bad input ~s" ast)]))

; String -> JSON
(define (string->json str)
  (parse-json-value (parse VALUE str)))




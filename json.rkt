#lang racket
(require "parse.rkt")

(provide string->json
         VALUE
         NUMBER
         NONZERO-DIGIT
         DIGIT
         STRING
         HEXIT
         ARRAY
         OBJECT
         PAIR
         SPACE)

; ----------------------
; ----- JSON Values -----
; ----------------------

; A JSON is one of:
; - 'null
; - Boolean
; - Number
; - String
; - JSONArray
; - JSONObject

; A JSONArray is a [Listof JSON], representing an ordered
; collection of values.

; A JSONObject is an immutable [HashEqof Symbol JSON], mapping each
; name to its value. If a name appears more than once, the last value
; wins. This is the shape Racket's `json` library uses for objects.

; String -> JSON
(define (string->json str)
  (parse VALUE str))

; -------------------------------------
; ----- JSON Context Free Grammar -----
; -------------------------------------

; Parser
(define-parser VALUE
  (spacey (alt* STRING
                NUMBER
                OBJECT
                ARRAY
                (seq* (const true) (lit "true"))
                (seq* (const false) (lit "false"))
                (seq* (const 'null) (lit "null")))))

; Parser
(define-parser NUMBER
  (seq* (lambda (sign integer fraction exponent)
          (string->number
           (apply string-append
                  "#e"
                  (flatten (list sign integer fraction exponent)))))
         (opt (lit "-"))
         (alt* (lit "0") (seq* list
                               NONZERO-DIGIT
                               (star DIGIT)))
         (opt (seq* list (lit ".") (plus DIGIT)))
         (opt (seq* list (alt* (lit "e") (lit "E"))
                    (opt (alt* (lit "+") (lit "-")))
                    (plus DIGIT)))))

; Parser
(define-parser NONZERO-DIGIT
  (alt* (lit "1") (lit "2") (lit "3") (lit "4") (lit "5") (lit "6") (lit "7")
         (lit "8") (lit "9")))

; Parser
(define-parser DIGIT
  (alt* (lit "0") NONZERO-DIGIT))

; Parser
; Plain text is matched a run at a time with `span`, rather than a
; character at a time, so a long string doesn't cost a parser step per
; character.
(define-parser STRING
  (seq* (lambda (open chars close) (apply string-append (flatten chars)))
        (lit "\"")
        (star (alt* (span string-char?)
                    (lit "\\\"")
                    (lit "\\\\")
                    (lit "\\/")
                    (lit "\\b")
                    (lit "\\f")
                    (lit "\\n")
                    (lit "\\r")
                    (lit "\\t")
                    (seq* list (lit "\\u") HEXIT HEXIT HEXIT HEXIT)))
        (lit "\"")))

; Char -> Boolean
; Whether the character can appear in a JSON string without an escape.
(define (string-char? c)
  (not (or (char=? c #\") (char=? c #\\) (char<? c #\space))))

; Parser
(define-parser HEXIT
  (alt* DIGIT (lit "a") (lit "b") (lit "c") (lit "d") (lit "e") (lit "f")
               (lit "A") (lit "B") (lit "C") (lit "D") (lit "E") (lit "F")))

; Parser
(define-parser ARRAY
  (unwrap (spacey (lit "["))
          (comma-separated VALUE)
          (spacey (lit "]"))))

; Parser
(define-parser OBJECT
  (seq* (lambda (open pairs close)
          (for/hasheq ([pair (in-list pairs)])
            (values (car pair) (cdr pair))))
        (spacey (lit "{"))
        (comma-separated PAIR)
        (spacey (lit "}"))))

; Parser
(define-parser PAIR
  (seq* (lambda (name colon value) (cons (string->symbol name) value))
        STRING
        (spacey (lit ":"))
        VALUE))

; Parser -> Parser
; Matches zero or more `p`s separated by commas, producing the list of
; their values.
(define (comma-separated p)
  (alt (seq cons
            p
            (star (seq (lambda (comma x) x) (spacey (lit ",")) p)))
       (emp)))

; Parser
; Matches one or more whitespace characters.
(define-parser SPACE
  (span json-space?))

; Char -> Boolean
(define (json-space? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline) (char=? c #\return)))

; Parser -> Parser
(define (spacey l)
  (unwrap (opt SPACE) l (opt SPACE)))

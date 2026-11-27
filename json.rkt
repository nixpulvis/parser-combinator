#lang racket
(require "parse.rkt"
         "lex.rkt"
         (submod "parse.rkt" positions))

(provide string->json
         json-lexer
         VALUE
         NUMBER
         STRING
         ARRAY
         OBJECT
         PAIR)

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
; A JSON text is a value with whitespace around it. Over `json-lexer`'s
; Source, the whitespace before the value belongs to its first Token, and
; that after it to the end of the text, which `parse` asks for.
(define (string->json str)
  (parse VALUE (json-lexer str)))

; -------------------------------------
; ----- JSON Context Free Grammar -----
; -------------------------------------

; JSON is parsed in two stages: `json-lexer`, at the end of this file,
; turns the text into Tokens, and the grammar below parses the Tokens.
; Only the lexer knows about whitespace, so the grammar never mentions it.
;
; VALUE, ARRAY, OBJECT and PAIR are that grammar, each with `json-lexer`
; around it, so they take a String, or the Source `json-lexer` makes, or
; work in any other grammar, of characters or of another lexer's Tokens.
; Like any Token, the first takes the whitespace before it, and the
; whitespace after the last is left to what comes next. The grammar
; itself, over JSON's Tokens alone, is provided by the `tokens` submodule.

(define-parser VALUE (with-lexer json-lexer JSON-VALUE))
(define-parser ARRAY (with-lexer json-lexer JSON-ARRAY))
(define-parser OBJECT (with-lexer json-lexer JSON-OBJECT))
(define-parser PAIR (with-lexer json-lexer JSON-PAIR))

(module+ tokens
  (provide JSON-VALUE
           JSON-ARRAY
           JSON-OBJECT
           JSON-PAIR))

; Parser
; Each kind of value begins with its own kind of Token, so `alt` runs
; only the parser for the kind the next Token is, rather than trying each
; in turn.
(define-parser JSON-VALUE
  (alt (tok 'null)
       (tok 'true)
       (tok 'false)
       (tok 'number)
       (tok 'string)
       JSON-ARRAY
       JSON-OBJECT))

; Parser
(define-parser JSON-ARRAY
  (unwrap (tok 'begin-array)
          (sep-by* JSON-VALUE (tok 'value-separator))
          (tok 'end-array)))

; Parser
(define-parser JSON-OBJECT
  (pmap (lambda (open pairs close)
          (for/hasheq ([pair (in-list pairs)])
            (values (car pair) (cdr pair))))
        (tok 'begin-object)
        (sep-by* JSON-PAIR (tok 'value-separator))
        (tok 'end-object)))

; Parser
(define-parser JSON-PAIR
  (pmap (lambda (name separator value)
          (cons (string->symbol name) value))
        (tok 'string)
        (tok 'name-separator)
        JSON-VALUE))

; -------------------------------
; ----- Numbers and Strings -----
; -------------------------------

; The lexer's rules for numbers and strings, written over characters.

; Parser
; The value is the exact number the text names: the integer and fraction
; digits as one integer, scaled by a power of 10. Building it takes time
; and memory in proportion to the scale, so a short text like
; 1e1000000000 could run out of memory. Beyond `exact-scale`, the value
; is the nearest float instead, as `string->jsexpr` would give, which
; RFC 8259 section 6 allows.
(define-parser NUMBER
  (pmap (lambda (sign int frac exponent)
          (let ([digits (string-append int frac)]
                [scale (- exponent (string-length frac))])
            (if (<= (abs scale) exact-scale)
                (let ([n (* (digits->natural digits) (expt 10 scale))])
                  (if (equal? sign "-") (- n) n))
                (string->number (string-append sign digits "e" (number->string scale))
                                10 'number-or-false 'decimal-as-inexact))))
        (opt (lit "-") "")
        (alt (lit "0") DIGITS)
        (opt (preceded (lit ".") DIGITS) "")
        (opt (pmap (lambda (e sign digits)
                     (if (equal? sign "-")
                         (- (digits->natural digits))
                         (digits->natural digits)))
                   (alt (lit "e") (lit "E"))
                   (opt (alt (lit "+") (lit "-")) "+")
                   DIGITS)
             0)))

; Natural
; The largest power of 10, up or down, that NUMBER scales an exact number
; by. Past it, NUMBER produces a float.
(define exact-scale 10000)

; String -> Natural
; The value of a string of decimal digits. Adding up one digit at a time
; is fastest for the short numbers JSON usually holds, but it multiplies
; a growing number for every digit, so past 64 digits `string->number`
; is faster.
(define (digits->natural str)
  (if (<= (string-length str) 64)
      (for/fold ([n 0]) ([c (in-string str)])
        (+ (* n 10) (- (char->integer c) (char->integer #\0))))
      (string->number str)))

; Parser
; Matches a run of digits.
(define DIGITS (span "0123456789"))

; Parser
; Plain text is matched a run at a time with `span`, rather than a
; character at a time, so a long string doesn't cost a parser step per
; character.
(define-parser STRING
  (pmap (lambda (open pieces close) (apply string-append pieces))
        (lit "\"")
        (star (alt ESCAPE (span string-char?)))
        (lit "\"")))

; Parser
; Matches an escape sequence, producing the decoded character as a
; String. The backslash is matched once, and `alt` runs only the escape
; that the letter after it begins, rather than trying each in turn.
(define-parser ESCAPE
  ; String String -> Parser
  ; Matches the letter of an escape, producing the decoded character.
  (let ([escaped (lambda (letter char) (pmap (const char) (lit letter)))])
    (preceded (lit "\\")
              (alt (escaped "\"" "\"")
                   (escaped "\\" "\\")
                   (escaped "/" "/")
                   (escaped "b" "\b")
                   (escaped "f" "\f")
                   (escaped "n" "\n")
                   (escaped "r" "\r")
                   (escaped "t" "\t")
                   UNICODE-ESCAPE))))

; Parser
; Matches the rest of a `\u` escape after its backslash, or of a UTF-16
; surrogate pair of them, producing the character it names as a String.
; A surrogate without its other half produces the replacement character
; U+FFFD. The `u` is a `lit`, so `alt` can tell that a `\u` escape begins
; with it.
(define-parser UNICODE-ESCAPE
  (preceded (lit "u")
            (alt (pmap (lambda (high low)
                         (string (integer->char
                                  (+ first-supplementary
                                     (arithmetic-shift (- high first-high-surrogate)
                                                       surrogate-bits)
                                     (- low first-low-surrogate)))))
                       (where high-surrogate? CODE-UNIT)
                       (preceded (lit "\\u") (where low-surrogate? CODE-UNIT)))
                 (pmap (lambda (n)
                         (string (if (surrogate? n)
                                     replacement-character
                                     (integer->char n))))
                       CODE-UNIT))))

; Parser
; Matches the four hexadecimal digits of a `\u` escape, producing the
; UTF-16 code unit they name as a Natural. It has a Run of its own, since
; `span` can't match a fixed number of characters, and fails where a
; digit is missing, at the first character that isn't one.
(define CODE-UNIT
  (parser
   (lambda (in i)
     (define s (input-text in))
     (let ([end (+ i 4)])
       (let loop ([j i] [n 0])
         (cond [(= j end) (matched n j)]
               [(and (< j (string-length s)) (hex-digit (string-ref s j)))
                => (lambda (d) (loop (add1 j) (+ (* n 16) d)))]
               [else (fail in j)]))))))

; Char -> [Maybe Natural]
; The value of `c` as a hexadecimal digit, or #f if it isn't one.
(define (hex-digit c)
  (cond [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
        [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
        [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
        [else #f]))

; Natural
; A `\u` escape names a UTF-16 code unit, which only reaches U+FFFF. A
; character from first-supplementary on, like U+1F600, takes two: a high
; surrogate, then a low surrogate, as in `\uD83D\uDE00`. Its code point
; minus first-supplementary has 20 bits, and each surrogate holds
; surrogate-bits of them, the high surrogate the top half.
(define first-high-surrogate #xD800)
(define last-high-surrogate #xDBFF)
(define first-low-surrogate #xDC00)
(define last-low-surrogate #xDFFF)
(define first-supplementary #x10000)
(define surrogate-bits 10)

; Char
; Stands in for a surrogate without its other half.
(define replacement-character #\uFFFD)

; Natural -> Boolean
(define (high-surrogate? n) (<= first-high-surrogate n last-high-surrogate))
(define (low-surrogate? n) (<= first-low-surrogate n last-low-surrogate))
(define (surrogate? n) (<= first-high-surrogate n last-low-surrogate))

; Char -> Boolean
; Whether the character can appear in a JSON string without an escape.
(define (string-char? c)
  (not (or (char=? c #\") (char=? c #\\) (char<? c #\space))))

; -----------------
; ----- Lexer -----
; -----------------

; Lexer
; NUMBER and STRING are parsers over characters, so a number's Token
; holds the Number, and a string's the decoded String. Unlike a parser,
; a lexer is made when it's defined, so it comes after its rules.
(define json-lexer
  (lexer #:skip " \t\n\r"
         ['begin-object (lit "{")]
         ['end-object (lit "}")]
         ['begin-array (lit "[")]
         ['end-array (lit "]")]
         ['value-separator (lit ",")]
         ['name-separator (lit ":")]
         ['null (pmap (const 'null) (lit "null"))]
         ['true (pmap (const #t) (lit "true"))]
         ['false (pmap (const #f) (lit "false"))]
         ['number NUMBER]
         ['string STRING]))

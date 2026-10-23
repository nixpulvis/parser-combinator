#lang racket
(require "parse.rkt"
         (submod "parse.rkt" positions))

(provide string->json
         VALUE
         NUMBER
         STRING
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
; The first character of a value says what kind it is, so `switch`
; runs only that kind's parser, rather than each being tried in turn.
(define-parser VALUE
  (spacey (switch [(#\n) (pmap (const 'null) (lit "null"))]
                  [(#\t) (pmap (const true) (lit "true"))]
                  [(#\f) (pmap (const false) (lit "false"))]
                  [(#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) NUMBER]
                  [(#\") STRING]
                  [(#\[) ARRAY]
                  [(#\{) OBJECT])))

; Parser
(define-parser NUMBER
  (pmap (lambda parts
          (string->number (apply string-append "#e" (flatten parts))))
        (opt (lit "-"))
        (alt (lit "0") DIGITS)
        (opt (seq (lit ".") DIGITS))
        (opt (seq (alt (lit "e") (lit "E"))
                  (opt (alt (lit "+") (lit "-")))
                  DIGITS))))

; Char -> Boolean
(define (digit? c) (char<=? #\0 c #\9))

; Parser
; Matches a run of digits.
(define DIGITS (span digit?))

; Parser
; Plain text is matched a run at a time with `span`, rather than a
; character at a time, so a long string doesn't cost a parser step per
; character.
(define-parser STRING
  (pmap (lambda (open pieces close) (apply string-append pieces))
        (lit "\"")
        (star (switch [(#\\) ESCAPE]
                      [else (span string-char?)]))
        (lit "\"")))

; Parser
; Matches an escape sequence, producing the character it stands for as a
; String. The backslash is matched once, and the letter after it chooses
; the rest with `switch`, rather than each escape being tried in turn.
(define-parser ESCAPE
  ; String String -> Parser
  ; Matches the letter of an escape, producing the character it stands for.
  (let ([escaped (lambda (letter char) (pmap (const char) (lit letter)))])
    (pmap (lambda (backslash c) c)
          (lit "\\")
          (switch [(#\") (escaped "\"" "\"")]
                  [(#\\) (escaped "\\" "\\")]
                  [(#\/) (escaped "/" "/")]
                  [(#\b) (escaped "b" "\b")]
                  [(#\f) (escaped "f" "\f")]
                  [(#\n) (escaped "n" "\n")]
                  [(#\r) (escaped "r" "\r")]
                  [(#\t) (escaped "t" "\t")]
                  [(#\u) UNICODE-ESCAPE]))))

; Parser
; Matches the rest of a `\u` escape after its backslash, or of a UTF-16
; surrogate pair of them, producing the character it names as a String.
; A surrogate without its other half produces the replacement character
; U+FFFD.
(define-parser UNICODE-ESCAPE
  (alt (pmap (lambda (high low)
               (string (integer->char
                        (+ first-supplementary
                           (arithmetic-shift (- high first-high-surrogate)
                                             surrogate-bits)
                           (- low first-low-surrogate)))))
             (where high-surrogate? CODE-UNIT)
             (pmap (lambda (backslash low) low)
                   (lit "\\")
                   (where low-surrogate? CODE-UNIT)))
       (pmap (lambda (n)
               (string (if (surrogate? n)
                           replacement-character
                           (integer->char n))))
             CODE-UNIT)))

; Parser
; Matches the `uXXXX` of a `\u` escape, after its backslash, producing
; the UTF-16 code unit it names as a Natural.
(define CODE-UNIT
  (parser
   (lambda (s i)
     (let ([end (+ i 5)])
       (if (and (<= end (string-length s))
                (char=? (string-ref s i) #\u))
           (let loop ([j (add1 i)] [n 0])
             (cond [(= j end) (matched n j)]
                   [(hex-digit (string-ref s j))
                    => (lambda (d) (loop (add1 j) (+ (* n 16) d)))]
                   [else 'error]))
           'error)))))

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

; Parser
(define-parser ARRAY
  (unwrap (spacey (lit "["))
          (comma-separated VALUE)
          (spacey (lit "]"))))

; Parser
(define-parser OBJECT
  (pmap (lambda (open pairs close)
          (for/hasheq ([pair (in-list pairs)])
            (values (car pair) (cdr pair))))
        (spacey (lit "{"))
        (comma-separated PAIR)
        (spacey (lit "}"))))

; Parser
(define-parser PAIR
  (pmap (lambda (name colon value) (cons (string->symbol name) value))
        STRING
        (spacey (lit ":"))
        VALUE))

; Parser -> Parser
; Matches zero or more `p`s separated by commas, producing the list of
; their values.
(define (comma-separated p)
  (alt (pmap cons
             p
             (star (pmap (lambda (comma x) x)
                         (spacey (lit ","))
                         p)))
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

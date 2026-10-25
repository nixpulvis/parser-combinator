#lang racket
(require "parse.rkt"
         (submod "parse.rkt" positions))

(provide string->json
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
(define (string->json str)
  (parse VALUE str))

; -------------------------------------
; ----- JSON Context Free Grammar -----
; -------------------------------------

; Parser
; Matches a value with any whitespace around it. As in JSON's grammar,
; the whitespace between tokens belongs to the values, so it's looked
; for once: a value skips the whitespace on both sides, and the
; punctuation around it skips none. Each kind of value begins with its
; own characters, so `alt` runs only the parser for the kind the first
; character says, rather than trying each in turn.
(define-parser VALUE
  (unwrap SKIP-SPACE
          (alt (pmap (const 'null) (lit "null"))
               (pmap (const true) (lit "true"))
               (pmap (const false) (lit "false"))
               NUMBER
               STRING
               ARRAY
               OBJECT)
          SKIP-SPACE))

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
; Matches an escape sequence, producing the character it stands for as a
; String. The backslash is matched once, and `alt` runs only the escape
; that the letter after it begins, rather than trying each in turn.
(define-parser ESCAPE
  ; String String -> Parser
  ; Matches the letter of an escape, producing the character it stands for.
  (let ([escaped (lambda (letter char) (pmap (const char) (lit letter)))])
    (pmap (lambda (backslash c) c)
          (lit "\\")
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
; U+FFFD.
(define-parser UNICODE-ESCAPE
  (alt (pmap (lambda (high low)
               (string (integer->char
                        (+ first-supplementary
                           (arithmetic-shift (- high first-high-surrogate)
                                             surrogate-bits)
                           (- low first-low-surrogate)))))
             (where CODE-UNIT high-surrogate?)
             (pmap (lambda (backslash low) low)
                   (lit "\\")
                   (where CODE-UNIT low-surrogate?)))
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
           'error)))
   (start (list #\u) #f)))

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
; Matches an array with any whitespace around it. Inside a value, the
; value has already skipped the whitespace before it, so that skip finds
; none.
(define-parser ARRAY
  (pmap (lambda (space open vs close space2) vs)
        SKIP-SPACE
        (lit "[")
        (comma-separated VALUE)
        (lit "]")
        SKIP-SPACE))

; Parser
; Matches an object with any whitespace around it, as for `ARRAY`.
(define-parser OBJECT
  (pmap (lambda (space open pairs close space2)
          (for/hasheq ([pair (in-list pairs)])
            (values (car pair) (cdr pair))))
        SKIP-SPACE
        (lit "{")
        (comma-separated PAIR)
        (lit "}")
        SKIP-SPACE))

; Parser
; Matches a pair with any whitespace around it.
(define-parser PAIR
  (pmap (lambda (space name space2 colon value)
          (cons (string->symbol name) value))
        SKIP-SPACE
        STRING
        SKIP-SPACE
        (lit ":")
        VALUE))

; Parser -> Parser
; Matches one or more `p`s separated by commas, producing the list of
; their values, or only whitespace, producing '() as `SKIP-SPACE` does.
; Each `p` skips the whitespace around itself, so the commas don't.
(define (comma-separated p)
  (alt (pmap cons
             p
             (star (pmap (lambda (comma x) x) (lit ",") p)))
       SKIP-SPACE))

; Parser
; Matches zero or more whitespace characters, producing '().
(define SKIP-SPACE (skip " \t\n\r"))

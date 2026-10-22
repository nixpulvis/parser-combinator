#lang racket

(provide parse
         (struct-out result)
         define-parser
         emp
         lit
         rx
         seq
         seq*
         alt
         alt*
         opt
         star
         plus
         where
         unwrap)

; ----------------------------------------
; ----- Context Free Grammar Library -----
; ----------------------------------------

; A Value is whatever a parser produces:
; - `emp` produces '()
; - `lit` and `rx` produce the matched String
; - `seq` and `seq*` produce whatever their constructor returns,
;   given their parsers' Values
; - `alt` and `alt*` produce the Value of the parser that matched
; - `opt` produces its parser's Value, or '() if its parser fails
; - `star` and `plus` produce the list of their parser's Values
; - `where` produces its parser's Value, when its condition is met
; - `unwrap` produces the Value of its middle parser

; Parsers that match from a position in their input, rather than from
; the start of a copy of what's left of it. The combinators below are
; made this way. A parser written with these works with them without
; copying the input. Require them with
; (require (submod parser-combinator/parse positions)).
(module positions racket/base
  (provide (struct-out result)
           (struct-out matched)
           (struct-out parser)
           run)

  ; A Result is one of:
  ; - 'error
  ; - (result Value String)
  (struct result (value remaining) #:transparent)

  ; A Parser is a [String -> Result]

  ; A Match is one of:
  ; - 'error
  ; - (matched Value Natural)
  ; where the Natural is the position in the input just after the match.
  (struct matched (value end) #:transparent)

  ; A Run is a [String Natural -> Match]
  ; that matches the String starting at the Natural position.

  ; The combinators call each other's Runs with positions, so the input
  ; isn't copied while parsing. Applying a `parser` to a String runs it
  ; from position 0, and copies the rest of the input once, for the
  ; Result's `remaining`.
  (struct parser (run)
    #:property prop:procedure
    (lambda (self s)
      (let ([m ((parser-run self) s 0)])
        (if (symbol? m)
            'error
            (result (matched-value m) (substring s (matched-end m)))))))

  ; Parser String Natural -> Match
  ; Runs any Parser from position `i` of `s`. A Parser that is a plain
  ; function is applied to a copy of the rest of the input.
  (define (run p s i)
    (if (parser? p)
        ((parser-run p) s i)
        (let ([r (p (substring s i))])
          (if (symbol? r)
              'error
              (matched (result-value r)
                       (- (string-length s)
                          (string-length (result-remaining r)))))))))

(require 'positions)

; Parser String -> Value
(define (parse p str)
  (let ([m (run p str 0)])
    (cond [(symbol? m)
           (error "bad input, got error")]
          [(< (matched-end m) (string-length str))
           (error "bad input, had leftover text"
                  (substring str (matched-end m)))]
          [else
           (matched-value m)])))

; Parser -> Parser : Transformation
; Makes things lazy. The body is built the first time the parser runs,
; rather than every time, so a grammar isn't rebuilt as it parses.
(define-syntax-rule (define-parser name body)
  (define name
    (let ([p #f])
      (parser (lambda (s i)
                (unless p (set! p body))
                (run p s i))))))

; -> Parser
(define (emp)
  (parser
   (lambda (s i)
     (matched '() i))))

; String -> Parser
(define (lit str)
  (let ([len (string-length str)])
    (parser
     (lambda (s i)
       (if (and (<= (+ i len) (string-length s))
                (for/and ([c (in-string str)]
                          [d (in-string s i)])
                  (char=? c d)))
           (matched str (+ i len))
           'error)))))

; Regexp -> Parser
; Matches `pattern` at the start of the input.
(define (rx pattern)
  (unless (regexp? pattern)
    (raise-argument-error 'rx "regexp?" pattern))
  (let ([anchored ((if (pregexp? pattern) pregexp regexp)
                   (string-append "^(?:" (object-name pattern) ")"))])
    (parser
     (lambda (s i)
       (let ([m (regexp-match-positions anchored s i)])
         (if m
             (matched (substring s i (cdar m)) (cdar m))
             'error))))))

; Symbol Any Natural -> Void
; Raises an error for `who` unless `build` is a constructor that accepts
; `n` Values.
(define (check-constructor who build n)
  (unless (and (procedure? build) (procedure-arity-includes? build n))
    (raise-argument-error who
                          (format "(procedure-arity-includes/c ~a)" n)
                          build)))

; (Value Value -> Value) Parser Parser -> Parser
(define (seq build p1 p2)
  (check-constructor 'seq build 2)
  (parser
   (lambda (s i)
     (let ([m1 (run p1 s i)])
       (if (symbol? m1)
           'error
           (let ([m2 (run p2 s (matched-end m1))])
             (if (symbol? m2)
                 'error
                 (matched (build (matched-value m1) (matched-value m2))
                          (matched-end m2)))))))))

; (Value ... -> Value) Parser Parser ... -> Parser
; The constructor gets one Value per parser, collected into a list by
; nesting `seq` with `cons`.
(define (seq* build p . ps)
  (check-constructor 'seq* build (add1 (length ps)))
  (seq (lambda (v vs) (apply build v vs))
       p
       (foldr (lambda (p rest) (seq cons p rest))
              (emp)
              ps)))

; Parser Parser -> Parser
(define (alt p1 p2)
  (parser
   (lambda (s i)
     (let ([m1 (run p1 s i)])
       (if (symbol? m1) (run p2 s i) m1)))))

; Parser Parser ... -> Parser
(define (alt* p . ps)
  (if (empty? ps)
      p
      (alt p (apply alt* ps))))

; Parser -> Parser
(define (opt p)
  (alt p (emp)))

; TODO: Stop when `p` matches without consuming input. Today a parser
; that can match the empty string, such as `(opt p)` or `(rx #rx"a*")`,
; makes `star` loop forever once it matches before the end of the input.
; Parser -> Parser
(define (star p)
  (parser
   (lambda (s i)
     (run (alt (seq cons p (parser (lambda (s i)
                                     (if (= i (string-length s))
                                         (matched '() i)
                                         (run (star p) s i)))))
               (emp))
          s i))))

; Parser -> Parser
(define (plus p)
  (seq cons p (star p)))

; Parser [Value -> Any] -> Parser
; Matches what `p` matches, but only when `ok?` accepts its Value.
(define (where p ok?)
  (parser
   (lambda (s i)
     (let ([m (run p s i)])
       (if (and (matched? m) (ok? (matched-value m))) m 'error)))))

; Parser Parser Parser -> Parser
(define (unwrap left body right)
  (seq* (lambda (l b r) b) left body right))

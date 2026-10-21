#lang racket

(provide define-parser
         lit
         rx
         alt
         alt*
         seq
         seq*
         opt
         emp
         plus
         star
         unwrap
         parse
         (struct-out result))

; ----------------------------------------
; ----- Context Free Grammar Library -----
; ----------------------------------------

; A Value is whatever a parser produces:
; - `lit` and `rx` produce the matched String
; - `emp` produces '()
; - `seq` and `seq*` produce whatever their constructor returns
; - `alt` and `alt*` produce the Value of the parser that matched
; - `opt` produces its parser's Value, or '() if its parser fails
; - `star` and `plus` produce the list of their parser's Values
; - `unwrap` produces the Value of its middle parser

; A Result is one of:
; - 'error
; - (result Value String)
(struct result (value remaining) #:transparent)

; A Parser is a [String -> Result]

; Parser -> Parser : Transformation
; Makes things lazy.
(define-syntax-rule (define-parser name body)
  (define (name s)
    (body s)))

; String -> Parser
(define (lit str)
  (lambda (s)
    (let ([len (string-length str)])
      (if (and (>= (string-length s) len)
               (string=? (substring s 0 len) str))
          (result str (substring s len))
          'error))))

; Regexp -> Parser
; Matches `pattern` at the start of the input.
(define (rx pattern)
  (unless (regexp? pattern)
    (raise-argument-error 'rx "regexp?" pattern))
  (let ([anchored ((if (pregexp? pattern) pregexp regexp)
                   (string-append "^(?:" (object-name pattern) ")"))])
    (lambda (s)
      (let ([m (regexp-match anchored s)])
        (if m
            (result (car m) (substring s (string-length (car m))))
            'error)))))

; Parser Parser -> Parser
(define (alt p1 p2)
  (lambda (s)
    (let ([r1 (p1 s)])
      (if (symbol? r1) (p2 s) r1))))

; Parser Parser ... -> Parser
(define (alt* parser . parsers)
  (lambda (s)
    ((if (empty? parsers)
            parser
            (alt parser (apply alt* parsers))) s)))

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
  (lambda (s)
    (let ([r1 (p1 s)])
      (if (symbol? r1)
          'error
          (let ([r2 (p2 (result-remaining r1))])
            (if (symbol? r2)
                'error
                (result (build (result-value r1) (result-value r2))
                        (result-remaining r2))))))))

; (Value ... -> Value) Parser Parser ... -> Parser
; The constructor gets one Value per parser, collected into a list by
; nesting `seq` with `cons`.
(define (seq* build parser . parsers)
  (check-constructor 'seq* build (add1 (length parsers)))
  (seq (lambda (v vs) (apply build v vs))
       parser
       (foldr (lambda (p rest) (seq cons p rest))
              (lambda (s) (result '() s))
              parsers)))

; Parser -> Parser
(define (opt parser)
  (lambda (s)
    (let ([r (parser s)])
      (if (symbol? r) (result '() s) r))))

; -> Parser
(define (emp)
  (lambda (s)
    (result '() s)))

; Parser -> Parser
(define (plus parser)
  (lambda (s)
    ((seq cons parser (star parser)) s)))

; TODO: Stop when `parser` matches without consuming input. Today a parser
; that can match the empty string, such as `(opt p)` or `(rx #rx"a*")`,
; makes `star` loop forever once it matches before the end of the input.
; Parser -> Parser
(define (star parser)
  (lambda (s)
    ((alt (seq cons parser (lambda (s) (if (string=? s "")
                                          (result '() "")
                                          ((star parser) s))))
         (emp)) s)))

; Parser Parser Parser -> Parser
(define (unwrap left body right)
  (seq* (lambda (l b r) b) left body right))

; Parser String -> Value
(define (parse parser str)
  (let ([r (parser str)])
    (cond [(symbol? r)
           (error "bad input, got error")]
          [(> (string-length (result-remaining r)) 0)
           (error "bad input, had leftover text" (result-remaining r))]
          [else
           (result-value r)])))

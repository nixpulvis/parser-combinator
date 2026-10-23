#lang racket

(provide parse
         (struct-out result)
         define-parser
         emp
         lit
         rx
         span
         seq
         alt
         alt*
         switch
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
; - `lit`, `rx` and `span` produce the matched String
; - `seq` produces whatever its constructor returns, given its parsers'
;   Values
; - `alt` and `alt*` produce the Value of the parser that matched
; - `switch` produces the Value of the parser it chose
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

; [Char -> Any] -> Parser
; Matches the longest run of one or more characters that `ok?` accepts.
; Unlike `rx`, it has no fixed cost per match, so it suits runs that are
; often short, like whitespace.
(define (span ok?)
  (parser
   (lambda (s i)
     (let loop ([j i])
       (if (and (< j (string-length s)) (ok? (string-ref s j)))
           (loop (add1 j))
           (if (= j i)
               'error
               (matched (substring s i j) j)))))))

; Symbol Any Natural -> Void
; Raises an error for `who` unless `build` is a constructor that accepts
; `n` Values.
(define (check-constructor who build n)
  (unless (and (procedure? build) (procedure-arity-includes? build n))
    (raise-argument-error who
                          (format "(procedure-arity-includes/c ~a)" n)
                          build)))

; (seq build p ...+) : Parser
; where build : (Value ... -> Value), with one argument per parser
; Runs the parsers in order, each from where the last one's match ended,
; and calls `build` with their Values. With one parser, `seq` converts
; its Value. A call to `seq` expands to code for its number of parsers,
; which calls `build` directly, so no list of the Values is made. Used as
; a value, such as with `apply`, `seq` is the procedure `seq-procedure`,
; which collects the Values in a list instead. Like a procedure, `seq`
; evaluates `build` and the parsers once, in order, when it is made.
(define-syntax (seq stx)
  (syntax-case stx ()
    [(_ build p0 p ...)
     (with-syntax ([(t ...) (generate-temporaries #'(p0 p ...))]
                   [n (length (syntax->list #'(p0 p ...)))])
       ; Binding through `values` keeps a `lambda` given as `build` or as a
       ; parser from being named after the temporary it's bound to.
       #'(let-values ([(b t ...) (values build p0 p ...)])
           (check-constructor 'seq b n)
           (parser (lambda (s i) (seq-steps s i b () (t ...))))))]
    [(_ . args) #'(seq-procedure . args)]
    [_ (identifier? stx) #'seq-procedure]))

; (seq-steps s i build (v ...) (p ...)) : Match
; Runs each `p` in order from position `i` of `s`, and then calls
; `build` with the Values `v ...` followed by theirs.
(define-syntax seq-steps
  (syntax-rules ()
    [(_ s i build (v ...) ())
     (matched (build v ...) i)]
    [(_ s i build (v ...) (p rest ...))
     (let ([m (run p s i)])
       (if (symbol? m)
           'error
           (let ([x (matched-value m)] [j (matched-end m)])
             (seq-steps s j build (v ... x) (rest ...)))))]))

; (Value ... -> Value) Parser ... -> Parser
; `seq` as a procedure, for when it isn't called directly.
(define seq-procedure
  (let ([seq (lambda (build p . ps)
               (check-constructor 'seq build (add1 (length ps)))
               (let ([all (cons p ps)])
                 (parser
                  (lambda (s i)
                    (let loop ([ps all] [i i] [vals '()])
                      (if (null? ps)
                          (matched (apply build (reverse vals)) i)
                          (let ([m (run (car ps) s i)])
                            (if (symbol? m)
                                'error
                                (loop (cdr ps)
                                      (matched-end m)
                                      (cons (matched-value m) vals))))))))))])
    seq))

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

; (switch [(Char ...) Parser] ...) : Parser
; (switch [(Char ...) Parser] ... [else Parser]) : Parser
; Looks at the next character without consuming it, and runs only the
; parser of the clause that lists it, or the `else` parser if no clause
; does or there is no character left. If the chosen parser fails, so
; does `switch`, without trying any other clause. That is like `alt`
; for alternatives that can only start with the characters listed for
; them, and costs one `case` instead of trying each alternative in turn.
; Like the other combinators, it evaluates its parser expressions once,
; when it is made, rather than every time it runs.
(define-syntax (switch stx)
  (syntax-case stx (else)
    [(_ [(c ...) p] ... [else q])
     (with-syntax ([(t ...) (generate-temporaries #'(p ...))])
       #'(let ([t p] ... [otherwise q])
           (parser
            (lambda (s i)
              (if (< i (string-length s))
                  (case (string-ref s i)
                    [(c ...) (run t s i)] ...
                    [else (run otherwise s i)])
                  (run otherwise s i))))))]
    [(_ [(c ...) p] ...)
     ; Without an `else`, a character no clause lists fails. Any other
     ; shape, such as a misplaced `else`, matches neither rule and is a
     ; syntax error, rather than being expanded again forever.
     #'(switch [(c ...) p] ... [else (parser (lambda (s i) 'error))])]))

; Parser -> Parser
(define (opt p)
  (alt p (emp)))

; Parser -> Parser
; Stops when `p` fails, or matches without consuming input, which would
; otherwise repeat forever. The parser is built once, and refers to
; itself for each further match.
(define (star p)
  (letrec ([step (parser
                  (lambda (s i)
                    (let ([m (run p s i)])
                      (if (and (matched? m) (> (matched-end m) i)) m 'error))))]
           [self (alt (seq cons step (parser (lambda (s i) (run self s i))))
                      (emp))])
    self))

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
  (seq (lambda (l b r) b) left body right))

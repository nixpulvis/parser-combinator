#lang racket

(provide parse
         (struct-out result)
         define-parser
         emp
         lit
         rx
         span
         seq
         pmap
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
; - `pmap` produces whatever its function returns, given its parsers'
;   Values
; - `seq` produces the list of its parsers' Values
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

; (Value ... -> Value) Parser Parser ... -> Parser
; Runs the parsers in order, each from where the last one's match ended,
; and produces `f` of their Values, one argument per parser, as `map`
; calls its function with one element per list. With one parser, it
; converts that parser's Value. Unless `f` accepts that many arguments,
; it raises an error when the parser is made, before any input is parsed.
(define (pmap f p . ps)
  (let* ([ps (cons p ps)] [n (length ps)])
    (unless (and (procedure? f) (procedure-arity-includes? f n))
      (raise-argument-error 'pmap (format "(procedure-arity-includes/c ~a)" n) f))
    (parser (all-of f ps))))

; Parser Parser ... -> Parser
; Runs the parsers in order, as `pmap` does, and produces the list of
; their Values.
(define (seq p . ps)
  (apply pmap list p ps))

; Natural
; The most parsers `all-of` writes a Run out for, with `steps`. Longer
; sequences use its loop. A written-out Run costs nothing to pick, since
; `all-of` picks it once, when the parser is made, but each one adds to
; the compiled code. Every sequence in the JSON grammar is shorter than
; that. The tests run every length up to 20, so they cover the loop as
; long as this stays below that.
(define-for-syntax written-out 6)

; (Value ... -> Value) [Listof Parser] -> Run
; Matches all of `ps`, in order, each from where the last one's match
; ended, and calls `build` with their Values. It fails if any of them
; does.
;
; The loop at the end does this for any number of parsers, but it
; collects the Values in a list, then reverses it and applies `build` to
; it, which allocates a list for every match. Most sequences are short,
; so for up to `written-out` parsers `all-of` instead uses a Run written
; for that number. That Run keeps each Value in a variable of its own and
; calls `build` with them as plain arguments, so a match allocates no
; list.
(define (all-of build ps)
  (or (written-out-run build ps)
      (lambda (s i)
        (let loop ([ps ps] [i i] [vs '()])
          (if (null? ps)
              (matched (apply build (reverse vs)) i)
              (let ([m (run (car ps) s i)])
                (if (symbol? m)
                    'error
                    (loop (cdr ps)
                          (matched-end m)
                          (cons (matched-value m) vs)))))))))

; (written-out-run build ps) : (or/c Run #f)
; The Run that `steps` writes out for as many parsers as `ps` has, or #f
; when it has more than `written-out`. It expands to a `match` with a
; case for each number of parsers from 1 to `written-out`, so for
; `written-out` 2 it's
;
;   (match ps
;     [(list p1) (lambda (s i) (steps s i build () (p1)))]
;     [(list p1 p2) (lambda (s i) (steps s i build () (p1 p2)))]
;     [_ #f])
(define-syntax (written-out-run stx)
  (syntax-case stx ()
    [(_ build ps)
     (with-syntax ([((p ...) ...)
                    (for/list ([n (in-range 1 (add1 written-out))])
                      (generate-temporaries (build-list n (lambda (k) 'p))))])
       #'(match ps
           [(list p ...) (lambda (s i) (steps s i build () (p ...)))]
           ...
           [_ #f]))]))

; (steps s i build (v ...) (p ...)) : Match
; Writes out the body of one of `all-of`'s Runs. It runs each `p` in
; order, the first from position `i` of `s`, and fails as soon as one
; does. Otherwise it calls `build` with the Values `v ...` followed by
; the Values of the `p`s, and ends where the last `p` ended. `all-of`
; starts it with no `v`s, and each step adds one. For example,
; (steps s i build () (a b)) expands to
;
;   (let ([m (run a s i)])
;     (if (symbol? m)
;         'error
;         (let ([x (matched-value m)] [j (matched-end m)])
;           (let ([m (run b s j)])
;             (if (symbol? m)
;                 'error
;                 (let ([x2 (matched-value m)] [j2 (matched-end m)])
;                   (matched (build x x2) j2)))))))
;
; Each step's `x`, `j` and `m` are new variables, which `syntax-rules`
; keeps apart, so no step's Value hides an earlier one's. They're
; written `x2` and `j2` above to show that. It's a macro only so the
; written-out Runs don't have to be written by hand.
(define-syntax steps
  (syntax-rules ()
    [(_ s i build (v ...) ())
     (matched (build v ...) i)]
    [(_ s i build (v ...) (p rest ...))
     (let ([m (run p s i)])
       (if (symbol? m)
           'error
           (let ([x (matched-value m)] [j (matched-end m)])
             (steps s j build (v ... x) (rest ...)))))]))

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
           [self (alt (pmap cons
                            step
                            (parser (lambda (s i) (run self s i))))
                      (emp))])
    self))

; Parser -> Parser
(define (plus p)
  (pmap cons p (star p)))

; Parser [Value -> Any] -> Parser
; Matches what `p` matches, but only when `ok?` accepts its Value.
(define (where p ok?)
  (parser
   (lambda (s i)
     (let ([m (run p s i)])
       (if (and (matched? m) (ok? (matched-value m))) m 'error)))))

; Parser Parser Parser -> Parser
(define (unwrap left body right)
  (pmap (lambda (l b r) b) left body right))

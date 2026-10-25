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
         opt
         star
         plus
         where
         unwrap
         preceded
         terminated)

; ----------------------------------------
; ----- Context Free Grammar Library -----
; ----------------------------------------

; A Value is whatever a parser produces:
; - `emp` produces its Value, '() unless it's given one
; - `lit`, `rx` and `span` produce the matched String
; - `pmap` produces whatever its function returns, given its parsers'
;   Values
; - `seq` produces the list of its parsers' Values
; - `alt` produces the Value of the parser that matched
; - `opt` produces its parser's Value, or its default if its parser
;   fails, '() unless it's given one
; - `star` and `plus` produce the list of their parser's Values
; - `where` produces its parser's Value, when its condition is met
; - `unwrap` produces the Value of its middle parser
; - `preceded` and `terminated` produce the Value of the parser they
;   don't skip

; A grammar refers to parsers that aren't defined yet, and to itself, so
; some work waits until the whole grammar exists, and is done the first
; time it's needed rather than when a parser is made: `define-parser`
; builds its body, a Start given as a thunk is worked out, and `alt`
; builds its table of which alternatives to try for each character.
; Each is done once and kept, in a variable or field set the first time.

; Parsers that match from a position in their input, rather than from
; the start of a copy of what's left of it. The combinators below are
; made this way. A parser written with these works with them without
; copying the input. Require them with
; (require (submod parser-combinator/parse positions)).
(module positions racket/base
  (provide (struct-out result)
           (struct-out matched)
           parser
           parser?
           parser-run
           run
           (struct-out start)
           start-of)

  (require (for-syntax racket/base))

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
  ; A parser also has its Start, below.
  (struct parser (run [start #:mutable])
    #:name parser-struct
    #:constructor-name make-parser
    #:property prop:procedure
    (lambda (self s)
      (let ([m ((parser-run self) s 0)])
        (if (symbol? m)
            'error
            (result (matched-value m) (substring s (matched-end m)))))))

  ; Run -> Parser
  ; Run Start -> Parser
  ; Makes a parser with the Start #f, or the given one, which can be a
  ; thunk. It's a macro so the struct is made where the Run is written,
  ; which Racket's compiler can see through. Made through a function
  ; instead, citm_catalog.json parses about 30% slower.
  (define-syntax (parser stx)
    (syntax-case stx ()
      [(_ run) #'(make-parser run #f)]
      [(_ run st) #'(make-parser run st)]))

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
                          (string-length (result-remaining r))))))))

  ; A Start says which characters a parser's matches can begin with, so
  ; `alt` can skip the alternatives that can't match the next one. It is
  ; one of:
  ; - #f, when it could be any character, or it isn't known
  ; - (start [Listof Char] Boolean), when it can only be one of the
  ;   Chars, and, if the Boolean is true, the parser can also match
  ;   without consuming anything
  (struct start (chars empty?))

  ; Any -> Start
  ; The Start of `p`. A plain function's Start is #f. While a Start is
  ; being worked out it's #f, so a grammar that refers to itself doesn't
  ; loop, and a Start worked out from it in the meantime keeps #f. That
  ; only turns `alt`'s dispatch off for that parser, so the tests check
  ; the JSON grammar's Starts.
  ; NOTE: Mutation gains 1.15x speed in the escapes bench, over keeping a
  ; thunk's Start in a promise made when the parser is.
  (define (start-of p)
    (if (parser? p)
        (let ([st (parser-start p)])
          (if (procedure? st)
              (begin
                (set-parser-start! p #f)
                (let ([st (st)])
                  (set-parser-start! p st)
                  st))
              st))
        #f)))

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

; (define-parser name body)
; Defines `name` as the parser `body`, built the first time it's needed.
; NOTE: Mutation gains 1.05x speed in the canada.json bench, and 1.3x in
; the escapes bench, over forcing a promise on every run.
(define-syntax-rule (define-parser name body)
  (define name
    (let* ([p #f]
           [build! (lambda () (unless p (set! p body)))])
      (parser (lambda (s i)
                (unless p (build!))
                (run p s i))
              (lambda ()
                (build!)
                (start-of p))))))

; The combinators from `emp` to `where` are the kernel. Each has its own
; Run, and the speed is all in these. The ones after `where` are made
; from the kernel's.

; -> Parser
; Value -> Parser
; Matches without consuming anything, producing `v`.
(define (emp [v '()])
  (parser
   (lambda (s i)
     (matched v i))
   (start '() #t)))

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
           'error))
     (if (= len 0)
         (start '() #t)
         (start (list (string-ref str 0)) #f)))))

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

; (or/c [Char -> Any] String) -> Parser
; Matches the longest run of one or more characters that `ok?` accepts,
; or, given a String, that are in it. Unlike `rx`, it has no fixed cost
; per match, so it suits runs that are often short, like whitespace.
(define (span ok?)
  (let ([accepts? (if (string? ok?) (char-in ok?) ok?)])
    (parser
     (lambda (s i)
       (let loop ([j i])
         (if (and (< j (string-length s)) (accepts? (string-ref s j)))
             (loop (add1 j))
             (if (= j i)
                 'error
                 (matched (substring s i j) j)))))
     (and (string? ok?) (start (string->list ok?) #f)))))

; String -> [Char -> Boolean]
; Whether a character is in `str`. ASCII characters are looked up in a
; table, so a long `str` costs no more than a short one.
(define (char-in str)
  (let* ([cs (string->list str)]
         [ascii (build-vector 128 (lambda (n) (and (memv (integer->char n) cs) #t)))]
         [others (filter (lambda (c) (>= (char->integer c) 128)) cs)])
    (lambda (c)
      (let ([n (char->integer c)])
        (if (< n 128)
            (vector-ref ascii n)
            (and (memv c others) #t))))))

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
    (parser (all-of f ps)
            (lambda () (seq-start ps)))))

; [Listof Parser] -> Start
; A sequence can begin with what its first parser can, and, if that one
; can match without consuming anything, with what the next one can, and
; so on.
(define (seq-start ps)
  (let loop ([ps ps] [chars '()])
    (if (null? ps)
        (start chars #t)
        (let ([st (start-of (car ps))])
          (cond [(not st) #f]
                [(start-empty? st)
                 (loop (cdr ps) (append chars (start-chars st)))]
                [else (start (append chars (start-chars st)) #f)])))))

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

; Parser ... -> Parser
; Tries each parser in order, from the same position, and matches what
; the first one to match does. It skips the parsers whose Start says
; they can't match the next character, so when each alternative begins
; with its own characters, it runs only one of them.
; NOTE: Mutation gains 1.1x speed in the canada.json bench, and 1.35x in
; the escapes bench, over forcing a promise on every run.
(define (alt . ps)
  (let ([choose #f])
    (parser
     (lambda (s i)
       (unless choose (set! choose (chooser ps)))
       ((choose s i) s i))
     (lambda () (alt-start ps)))))

; [Listof Parser] -> [String Natural -> Run]
; Picks the Run that tries just the parsers in `ps` that could match from
; position `i` of `s`, by the next character. A Run for each ASCII
; character is kept in a vector, and for other characters in a hash.
(define (chooser ps)
  (let* ([sts (map start-of ps)]
         [chars (remove-duplicates
                 (append-map (lambda (st) (if st (start-chars st) '())) sts))]
         ; [Maybe Char] -> Run
         [for-char
          (lambda (c)
            (first-of (for/list ([p (in-list ps)] [st (in-list sts)]
                                 #:when (or (not st)
                                            (start-empty? st)
                                            (and c (memv c (start-chars st)))))
                        p)))]
         [otherwise (for-char #f)]
         [ascii (for/vector #:length 128 ([n (in-range 128)])
                  (let ([c (integer->char n)])
                    (if (memv c chars) (for-char c) otherwise)))]
         [unicode (for/hasheqv ([c (in-list chars)]
                                #:when (>= (char->integer c) 128))
                    (values c (for-char c)))])
    (lambda (s i)
      (if (< i (string-length s))
          (let ([n (char->integer (string-ref s i))])
            (if (< n 128)
                (vector-ref ascii n)
                (hash-ref unicode (string-ref s i) (lambda () otherwise))))
          otherwise))))

; [Listof Parser] -> Run
; Matches what the first of `ps` to match does.
(define (first-of ps)
  (cond [(null? ps) (lambda (s i) 'error)]
        [(and (null? (cdr ps)) (parser? (car ps))) (parser-run (car ps))]
        [else (lambda (s i)
                (let loop ([ps ps])
                  (if (null? ps)
                      'error
                      (let ([m (run (car ps) s i)])
                        (if (symbol? m) (loop (cdr ps)) m)))))]))

; [Listof Parser] -> Start
; Alternatives can begin with what any of them can.
(define (alt-start ps)
  (let ([sts (map start-of ps)])
    (and (andmap values sts)
         (start (append-map start-chars sts) (ormap start-empty? sts)))))

; Parser -> Parser
; Stops when `p` fails, or matches without consuming input, which would
; otherwise repeat forever. The parser is built once, and refers to
; itself for each further match.
(define (star p)
  (letrec ([step (parser
                  (lambda (s i)
                    (let ([m (run p s i)])
                      (if (and (matched? m) (> (matched-end m) i)) m 'error)))
                  (lambda ()
                    (let ([st (start-of p)])
                      (and st (start (start-chars st) #f)))))]
           [self (alt (pmap cons
                            step
                            (parser (lambda (s i) (run self s i))))
                      (emp))])
    self))

; Parser [Value -> Any] -> Parser
; Matches what `p` matches, but only when `ok?` accepts its Value.
(define (where p ok?)
  (parser
   (lambda (s i)
     (let ([m (run p s i)])
       (if (and (matched? m) (ok? (matched-value m))) m 'error)))
   (lambda () (start-of p))))

; The combinators below are made from the kernel's.

; Parser Parser ... -> Parser
; Runs the parsers in order, as `pmap` does, and produces the list of
; their Values.
(define (seq p . ps)
  (apply pmap list p ps))

; Parser -> Parser
; Parser Value -> Parser
; Matches what `p` matches, or else nothing, producing `default`.
(define (opt p [default '()])
  (alt p (emp default)))

; Parser -> Parser
(define (plus p)
  (pmap cons p (star p)))

; Parser Parser Parser -> Parser
(define (unwrap left body right)
  (pmap (lambda (l b r) b) left body right))

; Parser Parser -> Parser
; Matches `skipped` and then `p`, producing `p`'s Value.
(define (preceded skipped p)
  (pmap (lambda (_ v) v) skipped p))

; Parser Parser -> Parser
; Matches `p` and then `skipped`, producing `p`'s Value.
(define (terminated p skipped)
  (pmap (lambda (v _) v) p skipped))

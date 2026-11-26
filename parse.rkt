#lang racket

(provide parse
         (struct-out result)
         define-parser
         emp
         lit
         rx
         span
         skip
         pmap
         seq
         alt
         opt
         star
         plus
         where
         unwrap
         preceded
         terminated
         sep-by*
         sep-by+
         tok
         line+column
         (struct-out token))

; ----------------------------------------
; ----- Context Free Grammar Library -----
; ----------------------------------------

; A Value is whatever a parser produces:
; - `emp` produces its Value, '() unless it's given one
; - `lit`, `rx` and `span` produce the matched String
; - `tok` produces the matched Token's value
; - `skip` produces '()
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
; - `sep-by*` and `sep-by+` produce the list of their parser's Values,
;   without the separators'

; What the library itself knows about a parser: its Run, which the
; `positions` module below shares with grammars, and its Start, which
; stays in here. A grammar never writes a Start. Each combinator works
; its own out from its parts, so `alt` can skip the alternatives that
; can't match the next character.
(module private racket/base
  (provide (struct-out result)
           (struct-out matched)
           (struct-out token)
           (struct-out lexed)
           lexed-value
           lexed-start
           lexed-kind+start
           lexed-end
           make-lexed
           peek!
           peek-id!
           kind-id
           kind-name
           kind-count
           input-length
           parser
           parser?
           parser-run
           run
           (struct-out start)
           start-of
           start-then
           start-or
           may-be-empty)

  (require (for-syntax racket/base))

  ; A Result is one of:
  ; - 'error
  ; - (result Value String)
  (struct result (value remaining) #:transparent)

  ; An Input is one of:
  ; - String, which the character parsers, like `lit`, match
  ; - Lexed, a String with a lexer, which `tok` matches a Token at a time
  ; The combinators only pass the Input and positions in it along, so
  ; they work over either. A grammar is written over one or the other.
  ; Positions in a Lexed are positions in its String, so a failure can
  ; say where it is in the source text.

  ; A Token is a (token Symbol Any Natural Natural)
  ; a piece of the source text a lexer recognized: its kind, its value,
  ; and where it begins and ends in the source text. `alt` picks its
  ; alternatives by the kind of the next Token, as it picks them by the
  ; next character in a String. Parsing doesn't make Tokens; only
  ; `tokens`, in lex.rkt, does, to show what a lexer sees.
  (struct token (kind value start end) #:transparent)

  ; A Lexed is a (lexed String Rule ...)
  ; a String, and a Rule, from lex.rkt, that skips what's between Tokens
  ; and matches the next one. Tokens
  ; are lexed when a parser asks for them, not ahead of time, and the
  ; last one is kept: `alt` asks for the next Token's kind, and the
  ; alternative it picks asks again for the same Token. So no Token is
  ; made, and each is usually lexed once.
  ; - `at` is the position the kept Token was asked for at, or #f
  ; - `kind` is its Kind ID, that of 'eof at the end, or #f if nothing
  ;   matches there
  ; - `match` is its rule's Match, with its value and where it ends,
  ;   which `tok` produces as it is, or 'error
  ; - `furthest` is the furthest position a Token was asked for at,
  ;   which is about where a failed parse failed
  ; Only what parsing needs is kept, so where a Token begins, and what's
  ; at `furthest`, are lexed again when they're needed.
  (require (only-in racket/vector vector-append))
  (struct lexed (string rule
                 [at #:mutable] [kind #:mutable] [match #:mutable]
                 [furthest #:mutable]))

  ; A Kind ID is a Natural that stands for a kind of Token, so that
  ; `alt` can look up the next Token's kind in a vector rather than a
  ; hash. Each kind gets one the first time a `tok` or lexer names it.
  (define kind-ids (make-hasheq))
  (define kind-names (vector))

  ; Symbol -> Natural
  (define (kind-id kind)
    (or (hash-ref kind-ids kind #f)
        (let ([id (hash-count kind-ids)])
          (hash-set! kind-ids kind id)
          (set! kind-names (vector-append kind-names (vector kind)))
          id)))

  ; (or/c Natural #f) -> (or/c Symbol #f)
  (define (kind-name id)
    (and id (vector-ref kind-names id)))

  ; -> Natural
  ; How many kinds have IDs.
  (define (kind-count) (hash-count kind-ids))

  ; Natural
  (define eof-id (kind-id 'eof))

  ; String Rule -> Lexed
  (define (make-lexed s rule)
    (lexed s rule #f #f 'error 0))

  ; Lexed Natural -> (values (or/c Natural #f) Natural)
  ; The Kind ID of the Token after position `i`, and where it begins,
  ; without keeping it.
  (define (lexed-kind+start in i)
    (let-values ([(id j m) ((lexed-rule in) (lexed-string in) i)])
      (values id j)))

  ; Lexed -> Natural
  ; Where the kept Token begins.
  (define (lexed-start in)
    (let-values ([(id j) (lexed-kind+start in (lexed-at in))])
      j))

  ; Lexed -> Any
  ; The kept Token's value.
  (define (lexed-value in) (matched-value (lexed-match in)))

  ; Lexed -> Natural
  ; Where the kept Token ends.
  (define (lexed-end in)
    (let ([m (lexed-match in)])
      (if (matched? m) (matched-end m) (lexed-start in))))

  ; Lexed Natural -> (or/c Symbol #f)
  ; The kind of the Token after position `i`, after what's skipped
  ; there, which it keeps along with its value and where it is.
  (define (peek! in i)
    (kind-name (peek-id! in i)))

  ; Lexed Natural -> (or/c Natural #f)
  ; The Kind ID of the Token after position `i`, as for `peek!`. The
  ; Lexed keeps Kind IDs, not kinds.
  ; It's a macro, so the check for the kept Token is written out where
  ; it's used, and only lexing a new Token calls a function.
  (define-syntax-rule (peek-id! in i)
    (let ([in* in] [i* i])
      (if (eq? i* (lexed-at in*))
          (lexed-kind in*)
          (lex! in* i*))))

  ; Lexed Natural -> (or/c Natural #f)
  ; Lexes the Token after position `i`, for `peek-id!`.
  (define (lex! in i)
    (let-values ([(id j m) ((lexed-rule in) (lexed-string in) i)])
      (set-lexed-at! in i)
      (set-lexed-kind! in id)
      (set-lexed-match! in m)
      (when (> i (lexed-furthest in))
        (set-lexed-furthest! in i))
      (lexed-kind in)))

  ; Input -> Natural
  (define (input-length s)
    (if (string? s) (string-length s) (string-length (lexed-string s))))

  ; A Parser is a [Input -> Result]
  ; that matches a prefix of the Input. The Result's `remaining` is the
  ; rest of the Input, after the match.

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
  ; A parser also has its Start, below, and an optional name, a Symbol or
  ; #f, which is its `object-name`, so it prints as #<procedure:name>.
  (struct parser (run [start #:mutable] name)
    #:name parser-struct
    #:constructor-name make-parser
    #:property prop:object-name 2
    #:property prop:procedure
    (lambda (self s)
      (let ([m ((parser-run self) s 0)])
        (if (symbol? m)
            'error
            (result (matched-value m)
                    (if (string? s)
                        (substring s (matched-end m))
                        (make-lexed (substring (lexed-string s) (matched-end m))
                                    (lexed-rule s))))))))

  ; Run -> Parser
  ; Run Start -> Parser
  ; Run Start Name -> Parser
  ; Makes a parser with the Start #f, or the given one, which can be a
  ; thunk, and the name #f, or the given one. It's a macro so the struct
  ; is made where the Run is written, which Racket's compiler can see
  ; through. Made through a function instead, citm_catalog.json parses
  ; about 30% slower.
  (define-syntax (parser stx)
    (syntax-case stx ()
      [(_ run) #'(make-parser run #f #f)]
      [(_ run st) #'(make-parser run st #f)]
      [(_ run st name) #'(make-parser run st name)]))

  ; Parser String Natural -> Match
  ; Runs any Parser from position `i` of `s`. A Parser that is a plain
  ; function is applied to a copy of the rest of the input. Only the
  ; length of its `remaining` is used, to find where its match ended, so
  ; it raises an error if `remaining` is longer than that copy.
  (define (run p s i)
    (if (parser? p)
        ((parser-run p) s i)
        (let* ([in (if (string? s)
                       (substring s i)
                       (raise-argument-error 'run "parser?" p))]
               [r (p in)])
          (cond [(symbol? r) 'error]
                [(> (input-length (result-remaining r)) (input-length in))
                 (raise-arguments-error
                  'run
                  "parser's remaining is longer than its input"
                  "parser" p
                  "input" in
                  "remaining" (result-remaining r))]
                [else
                 (matched (result-value r)
                          (- (input-length s)
                             (input-length (result-remaining r))))]))))

  ; A Start says which characters, or which kinds of Token, a parser's
  ; matches can begin with. It is one of:
  ; - #f, when it could be anything, or it isn't known
  ; - (start [Listof (or/c Char Symbol)] Boolean), when it can only be
  ;   one of the Chars, or a Token of one of the kinds, and, if the
  ;   Boolean is true, the parser can also match without consuming
  ;   anything
  ; So (start '() #t) is the Start of a parser that only matches nothing,
  ; like `emp`, and (start '() #f) of one that never matches.
  (struct start (chars empty?))

  ; Any -> Start
  ; The Start of `p`. A plain function's Start is #f, and so is that of a
  ; parser with a Run written by hand. A grammar refers to parsers that
  ; aren't defined yet, so a combinator gives its Start as a thunk, which
  ; is called the first time the Start is needed and replaced by what it
  ; returns. While a Start is being worked out it's #f, so a grammar that
  ; refers to itself doesn't loop, and a Start worked out from it in the
  ; meantime keeps #f. That only turns `alt`'s dispatch off for that
  ; parser, so the tests check the JSON grammar's Starts.
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
        #f))

  ; The Starts of the combinators are made from their parsers' with
  ; these three.

  ; Start [-> Start] -> Start
  ; The Start of a parser with the Start `a`, followed by one whose Start
  ; `rest` returns. It begins like the first, and, if the first can match
  ; without consuming anything, like the second too. `rest` is only
  ; called in that case, so a sequence doesn't ask for the Starts of the
  ; parsers after the first one that must consume something.
  (define (start-then a rest)
    (cond [(not a) #f]
          [(start-empty? a)
           (let ([b (rest)])
             (and b (start (append (start-chars a) (start-chars b))
                           (start-empty? b))))]
          [else a]))

  ; Start Start -> Start
  ; The Start of a choice between a parser with the Start `a` and one
  ; with the Start `b`, which begins like either.
  (define (start-or a b)
    (and a b (start (append (start-chars a) (start-chars b))
                    (or (start-empty? a) (start-empty? b)))))

  ; Start -> Start
  ; The Start of a parser that begins like one with the Start `a`, or
  ; else matches without consuming anything.
  (define (may-be-empty a)
    (and a (start (start-chars a) #t))))

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
           run)

  (require (rename-in (submod ".." private) [parser parser-with-start]))

  ; Run -> Parser
  ; Run Name -> Parser
  ; Makes a parser from a Run written by hand, with the name #f, or the
  ; given one. `alt` always tries it, since it can't tell what such a
  ; parser begins with.
  (define-syntax parser
    (syntax-rules ()
      [(_ run) (parser-with-start run)]
      [(_ run name) (parser-with-start run #f name)])))

(require 'private)

; Parser Input -> Value
(define (parse p in)
  (if (string? in)
      (let ([m (run p in 0)])
        (cond [(symbol? m)
               (error "bad input, got error")]
              [(< (matched-end m) (string-length in))
               (error "bad input, had leftover text"
                      (substring in (matched-end m)))]
              [else
               (matched-value m)]))
      (let ([m (run p in 0)])
        (if (and (matched? m) (eq? (peek! in (matched-end m)) 'eof))
            (matched-value m)
            (let*-values ([(id at) (lexed-kind+start
                                    in
                                    (if (matched? m) (matched-end m) (lexed-furthest in)))]
                          [(line column) (line+column (lexed-string in) at)])
              (error 'parse "unexpected ~a at line ~a, column ~a"
                     (case (kind-name id)
                       [(eof) "end of input"]
                       [(#f) (format "text ~s" (let ([s (lexed-string in)])
                                                 (substring s at (min (string-length s) (+ at 10)))))]
                       [else (kind-name id)])
                     line column))))))

; String Natural -> (values Natural Natural)
; The line and column, both from 1, of position `i` in `s`. It's worked
; out only when it's needed, for an error message.
(define (line+column s i)
  (for/fold ([line 1] [column 1]) ([c (in-string s 0 i)])
    (if (char=? c #\newline)
        (values (add1 line) 1)
        (values line (add1 column)))))

; (once x expr)
; The value of the variable `x`, which is #f until this first sets it to
; the value of `expr`. A grammar refers to parsers that aren't defined
; yet, and to itself, so some work has to wait until the whole grammar
; exists: `define-parser` builds its body, and `alt` its table of which
; alternatives to try for each character. Both do it with this, the
; first time they run, and keep the result.
; NOTE: Setting a variable gains 1.05x to 1.1x speed in the canada.json
; bench, and 1.3x to 1.35x in the escapes bench, over forcing a promise
; on every run.
(define-syntax-rule (once x expr)
  (or x (begin (set! x expr) x)))

; (define-parser name body)
; Defines `name` as the parser `body`, built the first time it's needed.
; The parser is named `name`.
(define-syntax-rule (define-parser name body)
  (define name
    (let ([p #f]
          [build (lambda () body)])
      (parser (lambda (s i)
                (run (once p (build)) s i))
              (lambda ()
                (start-of (once p (build))))
              'name))))

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
                (let loop ([k 0])
                  (or (= k len)
                      (and (char=? (string-ref str k) (string-ref s (+ i k)))
                           (loop (add1 k))))))
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
  (parser
   (scan ok? (s i j)
     (if (= j i)
         'error
         (matched (substring s i j) j)))
   (and (string? ok?) (start (string->list ok?) #f))))

; (or/c [Char -> Any] String) -> Parser
; Like `span`, but matches zero or more characters, and produces '()
; rather than copying them, for text a grammar only skips over.
(define (skip ok?)
  (parser
   (scan ok? (s i j)
     (matched '() j))
   (and (string? ok?) (start (string->list ok?) #t))))

; (scan ok? (s i j) body) : Run
; Makes a Run that finds where the run of characters that `ok?` accepts,
; or that are in it, ends, from position `i` of `s`, and then evaluates
; `body` with that position as `j`. Given a String, its ASCII characters
; are looked up in a table. The loop is written out in each Run, not
; called, so a Run that skips nothing costs no more than a check.
(define-syntax-rule (scan ok? (s i j) body)
  (if (string? ok?)
      (let-values ([(ascii others) (char-table ok?)])
        (lambda (s i)
          (let ([j (let loop ([j i])
                     (if (< j (string-length s))
                         (let* ([c (string-ref s j)] [n (char->integer c)])
                           (if (if (< n 128) (vector-ref ascii n) (memv c others))
                               (loop (add1 j))
                               j))
                         j))])
            body)))
      (lambda (s i)
        (let ([j (let loop ([j i])
                   (if (and (< j (string-length s)) (ok? (string-ref s j)))
                       (loop (add1 j))
                       j))])
          body))))

; String -> (values [Vectorof Boolean] [Listof Char])
; A table of which ASCII characters are in `str`, and its other
; characters.
(define (char-table str)
  (let ([cs (string->list str)])
    (values (build-vector 128 (lambda (n) (and (memv (integer->char n) cs) #t)))
            (filter (lambda (c) (>= (char->integer c) 128)) cs))))

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
  (if (null? ps)
      (start '() #t)
      (start-then (start-of (car ps))
                  (lambda () (seq-start (cdr ps))))))

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
(define (alt . ps)
  (let ([choose #f])
    (parser
     (lambda (s i)
       (((once choose (chooser ps)) s i) s i))
     (lambda () (alt-start ps)))))

; [Listof Parser] -> [Input Natural -> Run]
; Picks the Run that tries just the parsers in `ps` that could match from
; position `i` of `s`, by the next character. A Run for each ASCII
; character is kept in a vector, and for other characters in a hash.
; In a Lexed, it looks up the kind of the next Token in the hash
; instead.
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
         ; The other characters, and the kinds of Token.
         [others (for/hasheqv ([c (in-list chars)]
                               #:unless (and (char? c) (< (char->integer c) 128)))
                   (values c (for-char c)))]
         ; A Run for each kind of Token, by its Kind ID.
         [kinds (let ([v (make-vector (kind-count) otherwise)])
                  (for ([k (in-list chars)] #:when (symbol? k))
                    (vector-set! v (kind-id k) (for-char k)))
                  v)])
    (lambda (s i)
      (if (string? s)
          (if (< i (string-length s))
              (let ([n (char->integer (string-ref s i))])
                (if (< n 128)
                    (vector-ref ascii n)
                    (hash-ref others (string-ref s i) (lambda () otherwise))))
              otherwise)
          (let ([id (peek-id! s i)])
            (if (and id (< id (vector-length kinds)))
                (vector-ref kinds id)
                otherwise))))))

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
  (for/fold ([st (start '() #f)]) ([p (in-list ps)])
    (start-or st (start-of p))))

; Parser -> Parser
; Stops when `p` fails, or matches without consuming input, which would
; otherwise repeat forever. The matches are collected in a loop, so a
; long repetition doesn't nest a parser call per match.
(define (star p)
  (parser
   (lambda (s i)
     (let loop ([i i] [acc '()])
       (let ([m (run p s i)])
         (if (and (matched? m) (> (matched-end m) i))
             (loop (matched-end m) (cons (matched-value m) acc))
             (matched (reverse acc) i)))))
   (lambda () (may-be-empty (start-of p)))))

; [Value -> Any] Parser -> Parser
; Matches what `p` matches, but only when `ok?` accepts its Value.
(define (where ok? p)
  (parser
   (lambda (s i)
     (let ([m (run p s i)])
       (if (and (matched? m) (ok? (matched-value m))) m 'error)))
   (lambda () (start-of p))))

; Symbol -> Parser
; Symbol Any -> Parser
; Matches one Token of the kind `kind`, producing its value. Given a
; `value` too, it only matches a Token whose value is `equal?` to it,
; such as a keyword among the identifiers. It works over a Lexed, as
; the character parsers above work over a String.
(define tok
  (case-lambda
    [(kind)
     (let ([id (kind-id kind)])
       (parser
        (lambda (s i)
          (if (eq? (peek-id! s i) id)
              (lexed-match s)
              'error))
        (start (list kind) #f)
        kind))]
    [(kind value)
     (let ([id (kind-id kind)])
       (parser
        (lambda (s i)
          (if (and (eq? (peek-id! s i) id)
                   (equal? (lexed-value s) value))
              (matched value (lexed-end s))
              'error))
        (start (list kind) #f)
        kind))]))

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

; Parser Parser -> Parser
; Matches one or more `p`s with a `sep` between each, producing the
; list of the `p`s' Values.
(define (sep-by+ p sep)
  (pmap cons p (star (preceded sep p))))

; Parser Parser -> Parser
; Matches zero or more `p`s with a `sep` between each, as `sep-by+`
; does, or else nothing, producing '().
(define (sep-by* p sep)
  (opt (sep-by+ p sep)))

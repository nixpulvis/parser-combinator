#lang scribble/manual

@(require "shared.rkt"
          (for-label racket/base
                     racket/contract/base
                     parser-combinator/parse
                     (submod parser-combinator/parse positions)
                     (only-in parser-combinator/lex tok with-lexer)
                     (only-in parser-combinator/json json-lexer)))

@(define Value @tech{Value})
@(define Parser @tech{Parser})
@(define Result @tech{Result})
@(define Input @tech{Input})

@title[#:tag "parse"]{Parsers and Combinators}

@defmodule[parser-combinator/parse #:packages ("parser-combinator")
           #:use-sources ((submod parser-combinator/parse positions))]

@defproc[(parse [p #,Parser] [in #,Input]) #,Value]{
 applies @Parser @racket[p] to the @Input @racket[in] and produces the
 resulting @|Value|.
 Unlike applying @racket[p] directly, @racket[parse] demands that
 @racket[p] consume all of @racket[in]. After a match that ends with
 that of a parser made by @racket[with-lexer], what its lexer skips can
 be left. It raises @racket[exn:fail] if
 @racket[p] fails or leaves text over. The error gives the line and
 column where the parse got furthest: the furthest position any parser
 failed at, or where @racket[p]'s match ended. It may add these fields,
 one per line:
@itemlist[
 @item{@tt{lexing:} when no token could be read there, the kind of
 token whose rule got furthest. The error points to where that rule
 stopped, and no other fields are shown, since the grammar plays no
 part in reading a token.}
 @item{@tt{expected:} what would have matched there: the text of a
 @racket[lit], a character of a @racket[span] given a string, the kind
 of a @racket[tok], the end of input, or the name of a named parser
 that could start there. Parsers given a predicate or a @Run,
 @racket[rx], and @racket[where] don't say what they expect.}
 @item{@tt{in:} the innermost parser named with @racket[define-parser],
 or by @racket[parser] with a name, that started before that point and
 was still running there.}
]

 To fill in these fields, @racket[parse] runs @racket[p] a second time,
 so the functions given to @racket[pmap] run twice when a parse fails.

@examples[#:eval parse-eval
(parse (lit "hi") "hi")
(eval:error (parse (lit "hi") "bye"))
(eval:error (parse (lit "hi") "high"))
(define-parser GREETING
  (seq (lit "hi ") (alt (lit "you") (lit "all"))))
(eval:error (parse GREETING "hi yall"))
]}

@; -----------------------------------------------------------------------------
@section[#:tag "data"]{Data Definitions}

@deftech{Value} : @racket[any/c]

A @Value is whatever a @Parser produces when it matches. The parsers in
 this package produce plain Racket values:
@itemlist[
 @item{@racket[emp] produces the @Value it's given, or @racket['()]}
 @item{@racket[lit], @racket[rx], and @racket[span] produce the matched
 text, a string}
 @item{@racket[skip] produces @racket['()]}
 @item{@racket[pmap] produces whatever its function returns, given its
 parsers' @|Value|s}
 @item{@racket[seq] produces the list of its parsers' @|Value|s}
 @item{@racket[alt] produces the @Value of the parser that matched}
 @item{@racket[opt] produces its parser's @Value, or its default if its
 parser fails}
 @item{@racket[star] and @racket[plus] produce the list of their
 parser's @|Value|s}
 @item{@racket[where] produces its parser's @|Value|, when its
 condition is met}
 @item{@racket[unwrap] produces the @Value of its middle parser}
 @item{@racket[preceded] and @racket[terminated] produce the @Value of
 the parser they don't skip}
 @item{@racket[sep-by*] and @racket[sep-by+] produce the list of their
 parser's @|Value|s, without the separators'}
 @item{@racket[tok] produces the value of the token it matched}
 @item{@racket[with-lexer] produces its parser's @Value}
]
The functions given to @racket[pmap] are where a grammar builds the data
 the program needs, such as a number, or a structure of its own.

@deftech{Input} : @racket[input?]

An @Input is the text a parser matches: a string, or what a
 @seclink["lex"]{lexer} such as @racket[json-lexer] makes from one. A
 lexer's @Input also splits the text into tokens. Parsers over
 characters, like @racket[lit], work on either kind, and a grammar over
 tokens needs a lexer's.

@defproc[(input? [v any/c]) boolean?]{
 returns @racket[#t] if @racket[v] is an @|Input|: a string, or what a
 lexer makes.}

@deftech{Result} : @racket[(or/c 'error result?)]

A @Result is one of:
@itemlist[
 @item{@racket['error], meaning the parser did not match}
 @item{@racket[(result _value _remaining)], meaning the parser matched a
 prefix of its input, producing @racket[_value], and left the rest of
 it, the @Input @racket[_remaining], unconsumed. It's a string when the
 input was a string, and otherwise what a lexer would make of the rest.}
]

@defstruct*[result ([value #,Value] [remaining #,Input]) #:transparent]{
 represents a successful match, as produced by any @Parser that matches.
 See @|Result|.}

@deftech{Parser} : @racket[parser?]

A @Parser is made by the combinators below, or by @racket[parser] from
 a @tech{Run} (see @secref["run-parsers"]). A combinator given anything
 else raises @racket[exn:fail:contract] when it makes its parser. A
 parser only ever matches a @emph{prefix} of its input. It is up to
 @racket[parse] to insist that a match covers the whole input.

Parsers are functions. A parser can be applied directly to an @Input,
 such as a string, producing a @|Result|:

@examples[#:eval parse-eval
((lit "hi") "high")
((lit "hi") "bye")
]

Parsers pass positions to one another instead of copying the input.
 Applying a parser copies the input once, for the @racket[_remaining]
 of its @|Result|.

@; -----------------------------------------------------------------------------
@section[#:tag "defining"]{Defining Parsers}

@defform[(define-parser id parser-expr)]{
 defines @racket[id] as a @Parser that behaves like @racket[parser-expr].

 Unlike @racket[define], @racket[define-parser] does not evaluate
 @racket[parser-expr] until @racket[id] is first run. This lets a
 parser refer to itself. It can also refer to parsers that are defined
 later in the module. After that first run, @racket[id] keeps the
 parser that @racket[parser-expr] produced, so @racket[parser-expr] is
 evaluated only once, rather than every time the parser runs.

 The parser's @racket[object-name] is @racket[id], so it prints with
 that name, and @racket[parse]'s error names it when the parse fails
 inside it.

@examples[#:eval parse-eval
(code:comment "Parser")
(code:comment "matches balanced parentheses, such as \"(()())\",")
(code:comment "producing a list for each pair")
(define-parser PARENS
  (star (unwrap (lit "(") PARENS (lit ")"))))

(parse PARENS "(()())")
(eval:error (parse PARENS "(()"))
PARENS
]}

@; -----------------------------------------------------------------------------
@section[#:tag "primitive"]{Primitive Parsers}

@defproc[(emp [v any/c '()]) #,Parser]{
 produces a @Parser that always succeeds without consuming any input.
 The @Value is @racket[v].

 @racket[emp] makes a parser, rather than being one, so it's written
 @racket[(emp)], with parentheses.

@examples[#:eval parse-eval
((emp) "abc")
((emp 0) "abc")
]}

@defproc[(lit [str string?]) #,Parser]{
 produces a @Parser that matches the text @racket[str] at the start of
 its input. The @Value is @racket[str].

@examples[#:eval parse-eval
((lit "-") "-1.35")
((lit "hi") "high")
((lit "high") "hi")
((lit "") "walrus")
]}

@defproc[(rx [pattern regexp?]) #,Parser]{
 produces a @Parser that matches @racket[pattern] at the start of its
 input. A match later in the input does not count. The @Value is the
 matched text. @racket[pattern] may be a @racket[regexp] or a
 @racket[pregexp], such as @racket[#rx"[^ab]"] or @racket[#px"\\d+"].

@examples[#:eval parse-eval
((rx #rx"[^ab]") "cab")
((rx #rx"[^ab]") "bad")
((rx #px"\\d+") "42x")
((rx #rx"a|b") "xb")
]}

@defproc[(span [ok? (or/c (-> char? any/c) string?)]) #,Parser]{
 produces a @Parser that matches the longest run of one or more
 characters at the start of its input that @racket[ok?] accepts. It
 fails if the first character isn't accepted, or if there is no input
 left. The @Value is the matched text. In place of a function,
 @racket[ok?] can be a string of the characters to accept.

@examples[#:eval parse-eval
((span char-alphabetic?) "abc123")
((span char-numeric?) "abc")
((span "01") "0110 2")
]

 A @racket[span] matches the same text as an @racket[rx] for a character
 class, such as @racket[(rx #rx"[a-z]+")], but costs less when the runs
 are short. Matching a regexp against a string has a cost every time,
 even when it matches one character or none, and @racket[span] only
 checks characters. Because it always takes the longest run, it can't
 match a fixed number of characters, which takes an @racket[rx] such as
 @racket[(rx #px"[0-9]{4}")].}

@defproc[(skip [ok? (or/c (-> char? any/c) string?)]) #,Parser]{
 produces a @Parser that matches the longest run of zero or more
 characters at the start of its input that @racket[ok?] accepts, with
 @racket[ok?] as for @racket[span]. It always succeeds, and the @Value
 is @racket['()]. It is for text that a grammar passes over, such as
 whitespace.

@examples[#:eval parse-eval
((skip " \t") "  \tx")
((skip " \t") "x")
]

 It matches what @racket[(pmap (lambda (_) '()) (opt (span ok?)))]
 does, without copying the text it skips.}

@; -----------------------------------------------------------------------------
@section[#:tag "combinator"]{Combinators}

@defproc[(pmap [f procedure?] [p #,Parser] ...+) #,Parser]{
 produces a @Parser that runs each @racket[p] in order, each on the
 input that the previous one left over. If any @racket[p] fails, the
 parser fails. The @Value is the result of calling @racket[f] with the
 @|Value|s of the @racket[p]s, one argument per parser, in order, as
 @racket[map] calls its function with one element of each list.

 With one @racket[p], @racket[f] converts its @|Value|, and with more,
 @racket[f] combines theirs. It can ignore any of them, which is useful
 for dropping delimiters. For the common cases, see @racket[unwrap],
 @racket[preceded], and @racket[terminated].

@examples[#:eval parse-eval
((pmap string->number (rx #px"\\d+")) "42!")
((pmap length (star (lit "a"))) "aab")
((pmap string-append (lit "a") (lit "b")) "abc")
((pmap string-append (lit "a") (lit "b")) "acb")
(parse (pmap (lambda (l x r) (string->symbol x))
             (lit "(") (rx #rx"[a-z]+") (lit ")"))
       "(abc)")
]

 @racket[f] must accept as many arguments as there are @racket[p]s. If
 it does not, @racket[pmap] raises @racket[exn:fail:contract] when it
 makes the parser, before any input is parsed.

@examples[#:eval parse-eval
(eval:error
 (pmap (lambda (a b) a) (lit "a") (lit "b") (lit "c")))
]}

@defproc[(seq [p #,Parser] ...+) #,Parser]{
 produces a @Parser that runs each @racket[p] in order. The @Value is
 the list of the @racket[p]s' @|Value|s, in order. It is the same as
 @racket[(pmap list p ...)].

@examples[#:eval parse-eval
((seq (lit "a") (lit "b")) "abc")
((seq (lit "a") (lit "b")) "acb")
]}

@defproc[(alt [p #,Parser] ...) #,Parser]{
 produces a @Parser that tries each @racket[p] in order, on the same
 input, and behaves like the first one that matches. If none of them
 matches, it fails, so with no parsers it always fails.

@examples[#:eval parse-eval
((alt (lit "a") (lit "b")) "bc")
((alt (lit "a") (lit "b")) "c")
((alt (lit "x") (lit "y") (lit "z")) "zebra")
]

 The choice is @emph{ordered} and final: once a @racket[p] matches,
 @racket[alt] never goes back to try the ones after it, even if the
 rest of the grammar then fails. When one alternative is a prefix of
 another, list the longer one first.

@examples[#:eval parse-eval
(eval:error
 (parse (seq (alt (lit "a") (lit "ab")) (lit "c"))
        "abc"))
(parse (seq (alt (lit "ab") (lit "a")) (lit "c"))
       "abc")
]

 @racket[alt] looks at the next character of its input, and skips the
 alternatives whose matches can't start with it. When each alternative
 starts with its own characters, for example the kinds of value in JSON,
 only one of them runs, however many there are. This never changes what
 @racket[alt] matches, only how many alternatives it tries.

 @racket[alt] can only skip an alternative when it knows which
 characters the alternative starts with. A @racket[lit] starts with the
 first character of its string, and a @racket[span] or @racket[skip]
 given a string starts with any character in that string. A parser
 built with the combinators starts like the parsers inside it, so
 @racket[(seq (lit "-") _p)] starts with @racket[#\-], and
 @racket[(alt (lit "a") (lit "b"))] with @racket[#\a] or @racket[#\b].

 Any other parser could start with any character, so @racket[alt]
 always tries it. That covers an @racket[rx], a @racket[span] or
 @racket[skip] given a function, and a parser made with
 @racket[parser]. It also covers a parser built with one of them at its
 start, such as @racket[(seq (rx #px"\\d") _p)].}

@defproc[(opt [p #,Parser] [default any/c '()]) #,Parser]{
 produces a @Parser that behaves like @racket[p] if @racket[p] matches,
 and otherwise succeeds without consuming input, producing
 @racket[default]. It is the same as @racket[(alt p (emp default))].

@examples[#:eval parse-eval
((opt (lit "-")) "-5")
((opt (lit "-")) "5")
((opt (lit "-") "+") "5")
]}

@defproc[(star [p #,Parser]) #,Parser]{
 produces a @Parser that matches @racket[p] zero or more times, as many
 times as possible. The @Value is the list of the @|Value|s of the
 matches, in order.

@examples[#:eval parse-eval
((star (lit "a")) "aab")
((star (lit "a")) "b")
]

 Like @racket[alt]'s choice, the matches are final: @racket[star] takes
 as many as it can, and never goes back to take fewer, even if the rest
 of the grammar then fails. So
 @racket[(seq (star (lit "a")) (lit "a"))] matches nothing at all,
 because the @racket[star] leaves no @racket["a"] for the @racket[lit].

 @racket[star] stops at the first match of @racket[p] that consumes no
 input, leaving that match out, so a @racket[p] that can match the empty
 string, such as @racket[(opt _q)], can't make it repeat forever.

@examples[#:eval parse-eval
((star (opt (lit "a"))) "aab")
]}

@defproc[(plus [p #,Parser]) #,Parser]{
 produces a @Parser that matches @racket[p] one or more times. The @Value
 is the list of the @|Value|s of the matches, as for @racket[star].

@examples[#:eval parse-eval
(parse (plus (lit "a")) "aa")
(eval:error (parse (plus (lit "a")) ""))
]}

@defproc[(where [ok? (-> #,Value any/c)] [p #,Parser]) #,Parser]{
 produces a @Parser that matches what @racket[p] matches, but only when
 @racket[ok?] accepts its @|Value|. Otherwise it fails where @racket[p]
 began, so a parse that fails there says what @racket[p] matched. The
 @Value is @racket[p]'s.

@examples[#:eval parse-eval
(define NUMBER (pmap string->number (rx #px"\\d+")))
(define BYTE (where byte? NUMBER))
(parse BYTE "200")
(eval:error (parse (seq (lit "x=") BYTE) "x=300"))
]}

@defproc[(unwrap [left #,Parser] [body #,Parser] [right #,Parser]) #,Parser]{
 produces a @Parser that runs @racket[left], @racket[body], and
 @racket[right] in order, and produces the @Value of @racket[body]. It is
 the same as @racket[(pmap (lambda (l b r) b) left body right)], and is
 useful for skipping over text that the program does not care about,
 such as whitespace.

@examples[#:eval parse-eval
(define SPACES (skip " "))
(parse (unwrap SPACES (lit "x") SPACES) "  x ")
]}

@defproc[(preceded [skipped #,Parser] [p #,Parser]) #,Parser]{
 produces a @Parser that runs @racket[skipped] and then @racket[p], and
 produces the @Value of @racket[p]. It is the same as
 @racket[(pmap (lambda (_ v) v) skipped p)], and is useful for dropping
 punctuation in front of a value.

@examples[#:eval parse-eval
(parse (preceded (lit "$") (rx #px"\\d+")) "$42")
]}

@defproc[(terminated [p #,Parser] [skipped #,Parser]) #,Parser]{
 produces a @Parser that runs @racket[p] and then @racket[skipped], and
 produces the @Value of @racket[p]. It is the same as
 @racket[(pmap (lambda (v _) v) p skipped)].

@examples[#:eval parse-eval
(parse (terminated (rx #px"\\d+") (lit "%")) "42%")
]}

@defproc[(sep-by* [p #,Parser] [sep #,Parser]) #,Parser]{
 produces a @Parser that matches @racket[p] zero or more times, with a
 @racket[sep] between each match. The @Value is the list of the
 @|Value|s of @racket[p]'s matches, in order, without @racket[sep]'s. It
 is the same as @racket[(opt (sep-by+ p sep))], so unlike
 @racket[sep-by+], it matches the empty string, producing @racket['()].

@examples[#:eval parse-eval
(define DIGITS (rx #px"\\d+"))
(parse (sep-by* DIGITS (lit ",")) "1,22,333")
(parse (sep-by* DIGITS (lit ",")) "")
(eval:error (parse (sep-by+ DIGITS (lit ",")) ""))
]

 A @racket[sep] is only matched along with the @racket[p] after it, so
 a trailing @racket[sep] is left unconsumed.

@examples[#:eval parse-eval
((sep-by* DIGITS (lit ",")) "1,22,")
]}

@defproc[(sep-by+ [p #,Parser] [sep #,Parser]) #,Parser]{
 produces a @Parser that matches @racket[p] one or more times, with a
 @racket[sep] between each match, as @racket[sep-by*] does. It is the
 same as @racket[(pmap cons p (star (preceded sep p)))].

@examples[#:eval parse-eval
(parse (sep-by+ DIGITS (lit ",")) "1,22")
(parse (sep-by+ DIGITS (lit ",")) "1")
]}

@; -----------------------------------------------------------------------------
@section[#:tag "run-parsers"]{Writing Parsers by Hand}

@(define Match @tech{Match})
@(define Run @tech{Run})

@defmodule[(submod parser-combinator/parse positions) #:packages ("parser-combinator")]

Every @Parser is made either by the combinators above, or by
 @racket[parser], out of a @Run written by hand. Prefer the combinators.
 They know what each parser can begin with, so @racket[alt] can skip
 alternatives that can't match, and they record where they fail, for
 @racket[parse]'s errors. Write a @Run for what the combinators can't
 express, such as a fixed count of characters, a grammar that depends
 on what it already matched, or a tight loop over characters.

@deftech{Run} : @racket[(-> input? exact-nonnegative-integer? (or/c 'error matched?))]

A @Run is given an @Input and a position in its text, matches from
 that position, and produces a @|Match|.

@deftech{Match} : @racket[(or/c 'error matched?)]

A @Match is one of:
@itemlist[
 @item{@racket['error], meaning the parser did not match}
 @item{@racket[(matched _value _end)], meaning the parser matched,
 producing @racket[_value], and the match ended just before position
 @racket[_end] of the input's text}
]

@defstruct*[matched ([value #,Value] [end exact-nonnegative-integer?]) #:transparent]{
 represents a successful match, as produced by a @Run that matches. See
 @|Match|.}

A @Run that follows these rules works like the combinators, in any
 grammar:
@itemlist[
 @item{It reads the text with @racket[input-text]. The @Input may be a
 lexer's, so it isn't necessarily a string.}
 @item{It produces @racket[(matched _value _end)], where @racket[_end]
 is the position just after its match, or @racket['error].}
 @item{It runs other parsers with @racket[run], passing its own @Input
 and a position, so they work over tokens as well as characters.}
 @item{When the text is wrong, it fails with @racket[fail] at the bad
 position, so @racket[parse]'s error points there. Producing
 @racket['error] instead fails at the position the @Run started from.}
]

@racket[alt] always tries a parser made with @racket[parser]. If all
 of a @Run's matches begin with the same text, match that text with a
 @racket[lit] first, and write the @Run for the rest, so @racket[alt]
 can skip it.

@defform*[((parser run-expr)
           (parser run-expr name-expr))
          #:contracts ([run-expr #,Run]
                       [name-expr (or/c symbol? #f)])]{
 makes a @Parser from a @|Run|. Applied to an @Input, the parser calls
 its @Run from position @racket[0], and turns the @Match into a
 @|Result|.

@examples[#:eval positions-eval
(code:comment "Char-Predicate -> Parser")
(code:comment "matches one character that `ok?` accepts")
(define (char-where ok?)
  (parser
   (lambda (in i)
     (define s (input-text in))
     (if (and (< i (string-length s))
              (ok? (string-ref s i)))
         (matched (string (string-ref s i)) (add1 i))
         'error))))
(parse (plus (char-where char-alphabetic?)) "abc")
((char-where char-numeric?) "7up")
]

 The value of @racket[name-expr] names the parser, as @racket[id] names
 one made with @racket[define-parser]. Without a name, or with
 @racket[#f], it prints as @racketresultfont{#<procedure>}.

@examples[#:eval positions-eval
(parser (lambda (in i) 'error) 'NEVER)
(parser (lambda (in i) 'error))
]

 @racket[parser] is a form, not a function, so it can't be passed to
 @racket[apply] or used as a value. A form makes the parser where its
 @Run is written, so Racket's compiler can see the @Run, which makes
 parsing faster.}

@defproc[(parser? [v any/c]) boolean?]{
 returns @racket[#t] if @racket[v] is a @|Parser|: one made by
 @racket[parser], or by one of the combinators.}

@defproc[(input-text [in #,Input]) string?]{
 produces the text of @racket[in], which a @Run matches from the
 position it's given. It returns the text itself, without copying it.}

@defproc[(run [p #,Parser] [in #,Input] [i exact-nonnegative-integer?])
         #,Match]{
 runs @racket[p] on @racket[in] from position @racket[i], producing a
 @|Match|. A @Run uses @racket[run] to run other parsers, passing its
 own input.

@examples[#:eval positions-eval
(code:comment "Parser -> Parser")
(code:comment "matches a digit n, then n of `p`, producing their list")
(define (counted p)
  (parser
   (lambda (in i)
     (let ([n (run (rx #rx"[0-9]") in i)])
       (if (symbol? n)
           'error
           (let loop ([k (string->number (matched-value n))]
                      [i (matched-end n)]
                      [vs '()])
             (if (zero? k)
                 (matched (reverse vs) i)
                 (let ([m (run p in i)])
                   (if (symbol? m)
                       'error
                       (loop (sub1 k)
                             (matched-end m)
                             (cons (matched-value m) vs)))))))))))
(run (counted (lit "ab")) "x2ababab" 1)
(parse (counted (lit "ab")) "3ababab")
(eval:error (parse (counted (lit "ab")) "2ababab"))
]}

@defproc[(fail [in #,Input] [i exact-nonnegative-integer?]) 'error]{
 produces @racket['error] for a @Run to fail with, and records that the
 parse got to position @racket[i] of @racket[in]. If the parse fails
 and gets no further, @racket[parse]'s error points there, as it does
 for the library's own parsers. This lets a @Run point at the bad
 character instead of where it started.

@examples[#:eval positions-eval
(code:comment "Parser")
(code:comment "matches two digits")
(define TWO-DIGITS
  (parser
   (lambda (in i)
     (define s (input-text in))
     (let loop ([j i])
       (cond [(= j (+ i 2))
              (matched (substring s i j) j)]
             [(and (< j (string-length s))
                   (char-numeric? (string-ref s j)))
              (loop (add1 j))]
             [else (fail in j)])))))
(parse TWO-DIGITS "42")
(eval:error (parse TWO-DIGITS "4x"))
]}

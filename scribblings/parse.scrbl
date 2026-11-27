#lang scribble/manual

@(require "shared.rkt"
          (for-label racket/base
                     racket/contract/base
                     parser-combinator/parse
                     (submod parser-combinator/parse positions)
                     (only-in parser-combinator/json json-lexer)))

@(define Value @tech{Value})
@(define Parser @tech{Parser})
@(define Result @tech{Result})
@(define Input @tech{Input})

@title[#:tag "parse"]{Parsers and Combinators}

@defmodule[parser-combinator/parse #:packages ("parser-combinator")
           #:use-sources ((submod parser-combinator/parse positions))]

@defproc[(parse [p #,Parser] [in input?]) #,Value]{
 applies @Parser @racket[p] to the @Input @racket[in] and produces the
 resulting @|Value|.
 Unlike applying @racket[p] directly, @racket[parse] demands that
 @racket[p] consume all of @racket[in]. It raises
 @racket[exn:fail] if @racket[p] fails or leaves text over.

@examples[#:eval parse-eval
(parse (lit "hi") "hi")
(eval:error (parse (lit "hi") "bye"))
(eval:error (parse (lit "hi") "high"))
]}

@; -----------------------------------------------------------------------------
@section[#:tag "data"]{Data Definitions}

@deftech{Value} : @racket[any/c]

A @Value is whatever a @Parser produces when it matches. The parsers in
 this library produce plain Racket values:
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
]
The functions given to @racket[pmap] are where a grammar builds the data
 the program needs, such as a number, or a structure of its own.

@deftech{Result} : @racket[(or/c 'error result?)]

A @Result is one of:
@itemlist[
 @item{@racket['error], meaning the parser did not match}
 @item{@racket[(result _value _remaining)], meaning the parser matched a
 prefix of its input, producing @racket[_value], and left the rest of
 it, the @Input @racket[_remaining], unconsumed. It's a string when the
 input was a string, and otherwise what a lexer would make of the rest.}
]

@deftech{Parser} : @racket[(-> input? (or/c 'error result?))]

A @Parser is a function that consumes an @Input and produces a
 @|Result|. A parser only ever matches a @emph{prefix} of its
 input. It is up to @racket[parse] to insist that a match covers the
 whole input.

Parsers are functions. A parser can be applied directly to a
 string:

@examples[#:eval parse-eval
((lit "hi") "high")
((lit "hi") "bye")
]

The parsers made by this library pass positions in the input to one
 another rather than copying it, so applying a parser copies the input
 only once, for the @racket[_remaining] string of its @|Result|. See
 @secref["run-parsers"] to write parsers that work the same way.

@defstruct*[result ([value #,Value] [remaining input?]) #:transparent]{
 represents a successful match, as produced by any @Parser that matches.
 See @|Result|.}

@deftech{Input} : @racket[input?]

An @Input is the text a parser matches: a string, or what a lexer
 makes from one, such as @racket[json-lexer], which also splits the text
 into tokens for a grammar over tokens. Parsers over characters, like
 @racket[lit], match the text of either. Only a grammar over tokens
 needs a lexer's.

@defproc[(input? [v any/c]) boolean?]{
 returns @racket[#t] if @racket[v] is an @|Input|: a string, or what a
 lexer makes.}

@; -----------------------------------------------------------------------------
@section[#:tag "defining"]{Defining Parsers}

@defform[(define-parser id parser-expr)]{
 defines @racket[id] as a @Parser that behaves like @racket[parser-expr].

 Unlike @racket[define], @racket[define-parser] does not evaluate
 @racket[parser-expr] until @racket[id] is first applied to a string.
 This lets a parser refer to itself. It can also refer to parsers that
 are defined later in the module. After that first application,
 @racket[id] keeps the parser that @racket[parser-expr] produced, so
 @racket[parser-expr] is evaluated only once, rather than every time the
 parser runs.

 The parser's @racket[object-name] is @racket[id], so it prints with
 that name.

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

 Note that @racket[emp] is a function that produces a parser, not a
 parser itself, so it is used as @racket[(emp)].

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
 @racket[skip] given a function, a parser written as a plain function,
 and one made with @racket[parser]. It also covers a parser built with
 one of them at its start, such as @racket[(seq (rx #px"\\d") _p)].}

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
 @racket[ok?] accepts its @|Value|. Otherwise it fails, as if
 @racket[p] had not matched. The @Value is @racket[p]'s.

@examples[#:eval parse-eval
(define NUMBER (pmap string->number (rx #px"\\d+")))
(define BYTE (where byte? NUMBER))
(parse BYTE "200")
(eval:error (parse BYTE "300"))
]

 It is the way to restrict a match by its @Value without writing a
 @Parser as a plain function, which would be given a copy of the rest of
 the input every time it runs. Written as a plain function,
 @racket[where] would be:

@examples[#:label #f #:eval parse-eval
(define (plain-where ok? p)
  (lambda (s)
    (let ([r (p s)])
      (if (and (result? r) (ok? (result-value r)))
          r
          'error))))
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
@section[#:tag "run-parsers"]{Run Parsers}

@(define Match @tech{Match})
@(define Run @tech{Run})

@defmodule[(submod parser-combinator/parse positions) #:packages ("parser-combinator")]

The parsers made by @racketmodname[parser-combinator/parse] each hold a
 @Run, which matches the input from a position, and call each other's
 @|Run|s rather than copying the input. This module provides those
 pieces, for a parser that can't be built from the combinators, such as
 one whose grammar depends on what it has already matched.

A @Parser written as a plain function also works with the combinators,
 but it is given a copy of the rest of the input every time it runs, so
 inside a loop like @racket[star] it makes parsing quadratic. A
 @racket[parser] written with this module doesn't copy.

A @Run reads its @Input's text with @racket[input-text], and passes the
 @Input on to @racket[run] as it is, so a parser written with this
 module works in a grammar over tokens, as well as one over characters.

@deftech{Match} : @racket[(or/c 'error matched?)]

A @Match is one of:
@itemlist[
 @item{@racket['error], meaning the parser did not match}
 @item{@racket[(matched _value _end)], meaning the parser matched,
 producing @racket[_value], and the match ended just before position
 @racket[_end] of the input's text}
]

@deftech{Run} : @racket[(-> input? exact-nonnegative-integer? (or/c 'error matched?))]

A @Run matches its input's text starting at the given position, and
 produces a @|Match|.

@defstruct*[matched ([value #,Value] [end exact-nonnegative-integer?]) #:transparent]{
 represents a successful match, as produced by a @Run that matches. See
 @|Match|.}

@defform*[((parser run-expr)
           (parser run-expr name-expr))
          #:contracts ([run-expr #,Run]
                       [name-expr (or/c symbol? #f)])]{
 makes a @Parser from a @|Run|. Applying it to a string calls its
 @Run from position @racket[0], and turns the @Match into a @|Result|,
 copying the rest of the input once, for its @racket[_remaining]. The
 combinators in @racketmodname[parser-combinator/parse] call its @Run
 directly, with positions, so used inside a grammar it never copies.

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

 The value of @racket[name-expr] is the parser's @racket[object-name],
 so the parser prints with that name. Without a name, or with
 @racket[#f], it prints as @racketresultfont{#<procedure>}.

@examples[#:eval positions-eval
(parser (lambda (in i) 'error) 'NEVER)
(parser (lambda (in i) 'error))
]

 @racket[parser] is a form, not a function, so it can't be passed to
 @racket[apply] or used as a value. A form makes the parser where its
 @Run is written, so Racket's compiler can see the @Run, which makes
 parsing faster.

 @racket[alt] can't tell which characters the matches of such a parser
 start with, so it always tries it. When they all start with the same
 text, match that text with a @racket[lit] first, and write a @Run for
 the rest.}

@defproc[(parser? [v any/c]) boolean?]{
 returns @racket[#t] if @racket[v] was made by @racket[parser], or by
 one of the combinators.}

@defproc[(parser-run [p parser?]) #,Run]{
 produces the @Run of @racket[p].}

@defproc[(input-text [in input?]) string?]{
 produces the text of @racket[in], which a @Run matches from the
 position it's given. It returns the text itself, without copying it.}

@defproc[(run [p #,Parser] [in #,Input] [i exact-nonnegative-integer?])
         (or/c 'error matched?)]{
 runs @racket[p] on @racket[in] from position @racket[i], producing a
 @|Match|. If @racket[p] is a @racket[parser], this calls its @|Run|.
 Otherwise @racket[p] is a plain function, and it is applied to a copy
 of @racket[in]'s text from @racket[i] on. A @Run uses @racket[run] to
 run other parsers, passing its own input.

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
]

 A plain function's match ends where its @racket[_remaining] begins,
 which @racket[run] finds from the length of @racket[_remaining] alone,
 not its text. So @racket[_remaining] must be the end of the string that
 the function was given. @racket[run] raises @racket[exn:fail:contract]
 if it is longer than that string. It doesn't detect other text of that
 length or shorter.

@examples[#:eval positions-eval
(code:comment "Parser")
(code:comment "matches nothing, but gives back more text than it got")
(define (GROW s)
  (result 'g (string-append "zz" s)))
(eval:error (run GROW "abc" 1))
]}

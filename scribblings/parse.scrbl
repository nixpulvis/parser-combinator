#lang scribble/manual

@(require "shared.rkt"
          (for-label racket/base
                     racket/contract/base
                     parser-combinator/parse
                     (submod parser-combinator/parse positions)))

@(define Value @tech{Value})
@(define Parser @tech{Parser})
@(define Result @tech{Result})

@title[#:tag "parse"]{Parsers and Combinators}

@defmodule[parser-combinator/parse #:packages ("parser-combinator")
           #:use-sources ((submod parser-combinator/parse positions))]

@defproc[(parse [p #,Parser] [str string?]) #,Value]{
 applies @Parser @racket[p] to @racket[str] and produces the resulting
 @|Value|.
 Unlike applying @racket[p] directly, @racket[parse] demands that
 @racket[p] consume all of @racket[str]. It raises
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
]
The functions given to @racket[pmap] are where a grammar builds the data
 the program needs, such as a number, or a structure of its own.

@deftech{Result} : @racket[(or/c 'error result?)]

A @Result is one of:
@itemlist[
 @item{@racket['error], meaning the parser did not match}
 @item{@racket[(result _value _remaining)], meaning the parser matched a
 prefix of its input, producing @racket[_value], and left the string
 @racket[_remaining] unconsumed}
]

@deftech{Parser} : @racket[(-> string? (or/c 'error result?))]

A @Parser is a function that consumes a string and produces a @|Result|.
 A parser only ever matches a @emph{prefix} of its input. It is up to
 @racket[parse] to insist that a match covers the whole string.

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

@defstruct*[result ([value #,Value] [remaining string?]) #:transparent]{
 represents a successful match, as produced by any @Parser that matches.
 See @|Result|.}

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

@examples[#:eval parse-eval
(code:comment "Parser")
(code:comment "matches balanced parentheses, such as \"(()())\",")
(code:comment "producing a list for each pair")
(define-parser PARENS
  (star (unwrap (lit "(") PARENS (lit ")"))))

(parse PARENS "(()())")
(eval:error (parse PARENS "(()"))
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
 characters at the start of its input that @racket[ok?] accepts, or,
 when @racket[ok?] is a string, that are in it. It fails if the first
 character isn't accepted, or if there is no input left. The @Value is
 the matched text.

@examples[#:eval parse-eval
((span char-alphabetic?) "abc123")
((span char-numeric?) "abc")
((span "01") "0110 2")
]

 Given a string, @racket[span] knows which characters it can start
 with, so @racket[alt] can skip it when the next character isn't one of
 them. Given a function, it can't know.

 A @racket[span] matches the same text as an @racket[rx] for a character
 class, such as @racket[(rx #rx"[a-z]+")], but costs less when the runs
 are short. Matching a regexp against a string has a cost every time,
 even when it matches one character or none, and @racket[span] only
 checks characters. Because it always takes the longest run, it can't
 match a fixed number of characters, which takes an @racket[rx] such as
 @racket[(rx #px"[0-9]{4}")].}

@; -----------------------------------------------------------------------------
@section[#:tag "combinator"]{Combinators}

@defproc[(pmap [f procedure?] [p #,Parser] ...+) #,Parser]{
 produces a @Parser that runs each @racket[p] in order, each on the
 input that the previous one left over. It fails if any @racket[p] does.
 The @Value is the result of calling @racket[f] with the @racket[p]s'
 @|Value|s, one argument per parser, just as @racket[map] calls its
 function with one element of each list. Unlike @racket[map]'s lists,
 the @racket[p]s are matched one after another, not side by side.

 With one @racket[p], @racket[f] converts its @Value into one of the
 program's own.

@examples[#:eval parse-eval
((pmap string->number (rx #px"\\d+")) "42!")
((pmap length (star (lit "a"))) "aab")
]

 With more, @racket[f] combines their @|Value|s. It can keep only some of
 them, which is useful for dropping delimiters. For the common case of
 keeping only the middle of three, see @racket[unwrap].

@examples[#:eval parse-eval
((pmap string-append (lit "a") (lit "b")) "abc")
((pmap string-append (lit "a") (lit "b")) "acb")
(parse (pmap (lambda (l x r) (string->symbol x))
             (lit "(") (rx #rx"[a-z]+") (lit ")"))
       "(abc)")
]

 @racket[f] must accept one argument per @racket[p]. If it does not,
 @racket[pmap] raises @racket[exn:fail:contract] when it makes the
 parser, before any input is parsed.

@examples[#:eval parse-eval
(eval:error
 (pmap (lambda (a b) a) (lit "a") (lit "b") (lit "c")))
]

 For a short sequence, as most are, @racket[f] is called with the
 @|Value|s directly, without collecting them in a list.
}

@defproc[(seq [p #,Parser] ...+) #,Parser]{
 produces a @Parser that runs each @racket[p] in order, as
 @racket[pmap] does. The @Value is the list of the @racket[p]s'
 @|Value|s, in order. It is the same as @racket[(pmap list p ...)].

@examples[#:eval parse-eval
((seq (lit "a") (lit "b")) "abc")
((seq (lit "a") (lit "b")) "acb")
]}

@defproc[(alt [p #,Parser] ...) #,Parser]{
 produces a @Parser that tries each @racket[p] in order, on the same
 input, and behaves like the first one that matches. It fails if none
 does, so with no parsers it always fails.

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

 Most parsers know which characters their matches can start with:
 @racket[(lit "null")] can only start with @racket[#\n], for example.
 @racket[alt] uses that to skip the alternatives that can't match the
 next character, without running them. So when each alternative starts
 with its own characters, as the kinds of value in JSON do, only one of
 them runs, however many there are. This never changes what
 @racket[alt] matches, only how many alternatives it tries, and those it
 tries are still tried in order. A parser knows what it can start with
 by these rules:
@itemlist[
 @item{@racket[lit] starts with the first character of its string, and
 @racket[(lit "")], like @racket[emp], matches without consuming
 anything}
 @item{@racket[span] given a string starts with one of its
 characters}
 @item{@racket[pmap] and @racket[seq] start with what their first
 parser can, and if that can match without consuming anything, also
 with what the next can, and so on}
 @item{@racket[alt] starts with what any of its parsers can}
 @item{@racket[opt] and @racket[star] start with what their parser can,
 and can also match without consuming anything}
 @item{@racket[plus] and @racket[where] start with what their parser
 can, @racket[unwrap], @racket[preceded] and @racket[terminated] like
 the @racket[pmap] they're made of, and a parser made by
 @racket[define-parser] with what its body can}
 @item{any other parser, such as an @racket[rx], a @racket[span] given a
 function, a plain function, or a @racket[parser] made without a
 @racket[start], could start with anything, so @racket[alt] always
 tries it}
]
 @racket[alt] works out which alternatives to try for each character
 the first time it runs, by when the whole grammar has been defined, so
 a grammar can refer to itself.
}

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

 Like @racket[alt], @racket[star] never gives back a match, so
 @racket[(seq (star (lit "a")) (lit "a"))] matches nothing at
 all. It stops at the first match of @racket[p] that consumes no input,
 leaving that match out, so a @racket[p] that can match the empty string,
 such as @racket[(opt _q)], can't make it repeat forever.

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

@defproc[(where [p #,Parser] [ok? (-> #,Value any/c)]) #,Parser]{
 produces a @Parser that matches what @racket[p] matches, but only when
 @racket[ok?] accepts its @|Value|. Otherwise it fails, as if
 @racket[p] had not matched. The @Value is @racket[p]'s.

@examples[#:eval parse-eval
(define NUMBER (pmap string->number (rx #px"\\d+")))
(define BYTE (where NUMBER byte?))
(parse BYTE "200")
(eval:error (parse BYTE "300"))
]

 It is the way to restrict a match by its @Value without writing a
 @Parser as a plain function, which would be given a copy of the rest of
 the input every time it runs. Written as a plain function,
 @racket[where] would be:

@examples[#:label #f #:eval parse-eval
(define (plain-where p ok?)
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
(define SPACES (star (lit " ")))
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
]

 The names and argument orders are those of @tt{preceded} and
 @tt{terminated} in Rust's nom library, which calls @racket[unwrap]
 @tt{delimited}.}

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

@deftech{Match} : @racket[(or/c 'error matched?)]

A @Match is one of:
@itemlist[
 @item{@racket['error], meaning the parser did not match}
 @item{@racket[(matched _value _end)], meaning the parser matched,
 producing @racket[_value], and the match ended just before position
 @racket[_end] of the input}
]

@deftech{Run} : @racket[(-> string? exact-nonnegative-integer? (or/c 'error matched?))]

A @Run matches its string starting at the given position, and produces
 a @|Match|.

@defstruct*[matched ([value #,Value] [end exact-nonnegative-integer?]) #:transparent]{
 represents a successful match, as produced by a @Run that matches. See
 @|Match|.}

@defform*[((parser run-expr)
           (parser run-expr start-expr))
          #:contracts ([run-expr #,Run]
                       [start-expr (or/c start? #f (-> (or/c start? #f)))])]{
 makes a @Parser from a @|Run|. Applying it to a string calls its
 @Run from position @racket[0], and turns the @Match into a @|Result|,
 copying the rest of the input once, for its @racket[_remaining]. The
 combinators in @racketmodname[parser-combinator/parse] call its @Run
 directly, with positions, so used inside a grammar it never copies.

 @racket[parser] is syntax, not a procedure, so it can't be passed to
 @racket[apply] or used as a value. Making the parser where its @Run is
 written lets Racket's compiler see the @Run, and every parser runs
 faster for it.

@examples[#:eval positions-eval
(code:comment "Char-Predicate -> Parser")
(code:comment "matches one character that `ok?` accepts")
(define (char-where ok?)
  (parser
   (lambda (s i)
     (if (and (< i (string-length s))
              (ok? (string-ref s i)))
         (matched (string (string-ref s i)) (add1 i))
         'error))))
(parse (plus (char-where char-alphabetic?)) "abc")
((char-where char-numeric?) "7up")
]

 With a @racket[start], the parser tells @racket[alt] which characters
 its matches can start with, as the combinators' parsers do. Without
 one, or given @racket[#f], it could start with anything. A thunk is
 called the first time it's needed, for a parser whose start depends on
 parsers that may not be defined yet.

@examples[#:eval positions-eval
(define VOWEL
  (parser
   (lambda (s i)
     (if (and (< i (string-length s))
              (memv (string-ref s i) (list #\a #\e #\i #\o #\u)))
         (matched (string (string-ref s i)) (add1 i))
         'error))
   (start (list #\a #\e #\i #\o #\u) #f)))
((alt VOWEL (lit "y")) "yes")
]}

@defproc[(parser? [v any/c]) boolean?]{
 returns @racket[#t] if @racket[v] was made by @racket[parser], or by
 one of the combinators.}

@defproc[(parser-run [p parser?]) #,Run]{
 produces the @Run of @racket[p].}

@defstruct*[start ([chars (listof char?)] [empty? boolean?])]{
 says which characters a parser's matches can start with: one of
 @racket[chars], or, if @racket[empty?] is true, none at all, because it
 can also match without consuming anything.}

@defproc[(start-of [p #,Parser]) (or/c start? #f)]{
 produces the @racket[start] of @racket[p], or @racket[#f] if it could
 start with anything, or if its start refers back to itself before it
 consumes anything.

@examples[#:eval positions-eval
(start-chars (start-of (alt (lit "a") (seq (opt (lit "-")) (lit "1")))))
(start-of (rx #rx"a"))
]}

@defproc[(run [p #,Parser] [str string?] [i exact-nonnegative-integer?])
         (or/c 'error matched?)]{
 runs @racket[p] on @racket[str] from position @racket[i], producing a
 @|Match|. If @racket[p] is a @racket[parser], this calls its @|Run|.
 Otherwise @racket[p] is a plain function, and it is applied to a copy of
 @racket[str] from @racket[i] on. A @Run uses @racket[run] to run other
 parsers.

@examples[#:eval positions-eval
(code:comment "Parser -> Parser")
(code:comment "matches a digit n, then n of `p`, producing their list")
(define (counted p)
  (parser
   (lambda (s i)
     (let ([n (run (rx #rx"[0-9]") s i)])
       (if (symbol? n)
           'error
           (let loop ([k (string->number (matched-value n))]
                      [i (matched-end n)]
                      [vs '()])
             (if (zero? k)
                 (matched (reverse vs) i)
                 (let ([m (run p s i)])
                   (if (symbol? m)
                       'error
                       (loop (sub1 k)
                             (matched-end m)
                             (cons (matched-value m) vs)))))))))))
(run (counted (lit "ab")) "x2ababab" 1)
(parse (counted (lit "ab")) "3ababab")
(eval:error (parse (counted (lit "ab")) "2ababab"))
]}

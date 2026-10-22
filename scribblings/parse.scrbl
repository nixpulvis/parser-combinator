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
 @item{@racket[emp] produces @racket['()]}
 @item{@racket[lit], @racket[rx], and @racket[span] produce the matched
 text, a string}
 @item{@racket[seq] and @racket[seq*] produce whatever their
 constructor returns, given their parsers' @|Value|s}
 @item{@racket[alt] and @racket[alt*] produce the @Value of the parser
 that matched}
 @item{@racket[opt] produces its parser's @Value, or @racket['()] if its
 parser fails}
 @item{@racket[star] and @racket[plus] produce the list of their
 parser's @|Value|s}
 @item{@racket[where] produces its parser's @|Value|, when its
 condition is met}
 @item{@racket[unwrap] produces the @Value of its middle parser}
]
The constructors are where a grammar builds the data the program needs,
 such as a list, a number, or a structure of its own.

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

@defproc[(emp) #,Parser]{
 produces a @Parser that always succeeds without consuming any input.
 The @Value is @racket['()].

 Note that @racket[emp] is a function that produces a parser, not a
 parser itself, so it is used as @racket[(emp)].

@examples[#:eval parse-eval
((emp) "abc")
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

@defproc[(span [ok? (-> char? any/c)]) #,Parser]{
 produces a @Parser that matches the longest run of one or more
 characters at the start of its input that @racket[ok?] accepts. It
 fails if the first character isn't accepted, or if there is no input
 left. The @Value is the matched text.

@examples[#:eval parse-eval
((span char-alphabetic?) "abc123")
((span char-numeric?) "abc")
]

 A @racket[span] matches the same text as an @racket[rx] for a character
 class, such as @racket[(rx #rx"[a-z]+")], but costs less when the runs
 are short. Matching a regexp against a string has a cost every time,
 even when it matches one character or none, and @racket[span] only
 checks characters. Because it always takes the longest run, it can't
 match a fixed number of characters, which takes an @racket[rx] such as
 @racket[(rx #px"[0-9]{4}")].}

@; -----------------------------------------------------------------------------
@section[#:tag "combinator"]{Combinators}

@defproc[(seq [build (-> #,Value #,Value #,Value)]
              [p1 #,Parser]
              [p2 #,Parser])
         #,Parser]{
 produces a @Parser that runs @racket[p1] and then runs @racket[p2] on
 the input that @racket[p1] left over. It fails if either parser does.
 The @Value is @racket[(build _left _right)], where @racket[_left] and
 @racket[_right] are the @|Value|s of @racket[p1] and @racket[p2].
 @racket[build] must accept two arguments. If it does not, @racket[seq]
 raises @racket[exn:fail:contract] when it makes the parser.

@examples[#:eval parse-eval
((seq list (lit "a") (lit "b")) "abc")
((seq string-append (lit "a") (lit "b")) "abc")
((seq list (lit "a") (lit "b")) "acb")
]}

@defproc[(seq* [build procedure?] [p #,Parser] ...+) #,Parser]{
 produces a @Parser that runs each @racket[p] in order, each on the
 input that the previous one left over. It fails if any @racket[p] does.
 The @Value is the result of calling @racket[build] with the @|Value|s of
 the @racket[p]s, one argument per parser, in order.

 @racket[build] must accept as many arguments as there are
 @racket[p]s. If it does not, @racket[seq*] raises
 @racket[exn:fail:contract] when it makes the parser, before any input
 is parsed. A constructor can keep only some of the @|Value|s, which is
 useful for dropping delimiters. For the common case of keeping only the
 middle of three, see @racket[unwrap].

@examples[#:eval parse-eval
(parse (seq* list (lit "a") (lit "b") (lit "c"))
       "abc")
(parse (seq* (lambda (l x r) (string->symbol x))
             (lit "(") (rx #rx"[a-z]+") (lit ")"))
       "(abc)")
(eval:error
 (seq* (lambda (a b) a) (lit "a") (lit "b") (lit "c")))
]}

@defproc[(alt [p1 #,Parser] [p2 #,Parser]) #,Parser]{
 produces a @Parser that tries @racket[p1] and, only if it fails,
 tries @racket[p2] on the same input.

@examples[#:eval parse-eval
((alt (lit "a") (lit "b")) "bc")
((alt (lit "a") (lit "b")) "c")
]

 The choice is @emph{ordered} and final: once @racket[p1] matches,
 @racket[alt] never goes back to try @racket[p2], even if the rest of
 the grammar then fails. When one alternative is a prefix of another,
 list the longer one first.

@examples[#:eval parse-eval
(eval:error
 (parse (seq list (alt (lit "a") (lit "ab")) (lit "c"))
        "abc"))
(parse (seq list (alt (lit "ab") (lit "a")) (lit "c"))
       "abc")
]}

@defproc[(alt* [p #,Parser] ...+) #,Parser]{
 produces a @Parser that tries each @racket[p] in order and behaves like
 the first one that matches. It is the same as nesting @racket[alt] to
 the right, and with a single parser @racket[(alt* p)] is the same as
 @racket[p].

@examples[#:eval parse-eval
((alt* (lit "x") (lit "y") (lit "z")) "zebra")
]}

@defproc[(opt [p #,Parser]) #,Parser]{
 produces a @Parser that behaves like @racket[p] if @racket[p] matches,
 and otherwise succeeds without consuming input, producing
 @racket['()].

@examples[#:eval parse-eval
((opt (lit "-")) "-5")
((opt (lit "-")) "5")
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
 @racket[(seq list (star (lit "a")) (lit "a"))] matches nothing at
 all. A parser @racket[p] that can succeed without consuming input, such
 as @racket[(opt _q)], makes @racket[(star p)] loop forever once
 @racket[p] matches the empty string before the end of the input.}

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
(define NUMBER (seq* string->number (rx #px"\\d+")))
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
 the same as @racket[(seq* (lambda (l b r) b) left body right)], and is
 useful for skipping over text that the program does not care about,
 such as whitespace.

@examples[#:eval parse-eval
(define SPACES (star (lit " ")))
(parse (unwrap SPACES (lit "x") SPACES) "  x ")
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

@defstruct*[parser ([run #,Run])]{
 is a @Parser made from a @|Run|. Applying it to a string calls its
 @Run from position @racket[0], and turns the @Match into a @|Result|,
 copying the rest of the input once, for its @racket[_remaining]. The
 combinators in @racketmodname[parser-combinator/parse] call its @Run
 directly, with positions, so used inside a grammar it never copies.

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

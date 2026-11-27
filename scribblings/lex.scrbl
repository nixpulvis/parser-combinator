#lang scribble/manual

@(require "shared.rkt"
          (for-label racket/base
                     racket/contract/base
                     parser-combinator/parse
                     parser-combinator/lex))

@(define Parser @tech{Parser})
@(define Input @tech{Input})
@(define Lexer @tech{Lexer})

@title[#:tag "lex"]{Lexers}

@defmodule[parser-combinator/lex #:packages ("parser-combinator")]

A lexer is the first stage of parsing. It splits text into tokens,
 such as numbers, names, and punctuation, and drops what the grammar
 doesn't need, such as whitespace and comments. A grammar written with
 @racket[tok] then parses the tokens and never mentions what the lexer
 dropped. A lexer's rules are @|Parser|s over characters, written with
 the same combinators as any other grammar.

The two stages run together: a token is read when the grammar asks for
 it, and no list of tokens is built. A lexer turns a string into an
 @|Input|, which @racket[parse] accepts as it does a string.

@deftech{Lexer} : @racket[(-> string? input?)]

A @Lexer makes an @Input of a string, whose tokens a grammar over
 tokens matches.

@defform*[((lexer [kind rule] ...)
           (lexer #:skip skip [kind rule] ...))
          #:contracts ([skip (or/c string? #,Parser)]
                       [rule #,Parser])]{
 produces a @|Lexer|. Each token comes after whatever @racket[skip]
 matches, or, when @racket[skip] is a string, after any run of its
 characters, which is faster. Without @racket[skip], nothing is skipped.
 A token is matched by the first @racket[rule] that matches some text,
 and has that rule's @racket[kind], any symbol, and the @racket[rule]'s
 @tech{Value} as its value. A rule that matches no text is passed over,
 since a token must have some; only @racket[skip] can match none.

@examples[#:eval lex-eval
(define sums
  (lexer #:skip " "
         [num (pmap string->number (rx #px"\\d+"))]
         [plus (lit "+")]))
(define SUM (sep-by+ (tok 'num) (tok 'plus)))
(parse SUM (sums "1 + 2 +3"))
(eval:error (parse SUM (sums "1 + + 2")))
]

 As in @racket[alt], the rules are tried in order, and the first match
 wins, even if a later rule would match more. So a rule for a keyword,
 placed before the rule for names, splits a name that starts with that
 keyword. To avoid that, lex keywords as names, and match them with
 @racket[tok]'s @racket[value].}

@defproc*[([(tok [kind symbol?]) #,Parser]
           [(tok [kind symbol?] [value any/c]) #,Parser])]{
 produces a @Parser that matches one token of kind @racket[kind], and
 produces its value. With @racket[value], it only matches a token whose
 value is @racket[equal?] to it, such as one keyword among the names.
 It needs an @Input made by a lexer. Given a string, it raises
 @racket[exn:fail].

 @racket[alt] chooses among its alternatives by the kind of the next
 token, as it does by the next character. When each alternative begins
 with a different kind of token, only one of them runs.

@examples[#:eval lex-eval
(parse (seq (tok 'num 1) (tok 'plus) (tok 'num))
       (sums "1+2"))
(eval:error (parse (tok 'num) "1"))
]

 A parser over characters, such as @racket[lit], also works in a grammar
 over tokens, at the same position. It matches exactly the text there,
 so it doesn't skip what the lexer would skip before a token.

@examples[#:eval lex-eval
(parse (seq (tok 'num) (lit "!")) (sums "5!"))
((seq (tok 'num) (lit "!")) (sums "5 !"))
]}

@defproc[(with-lexer [lex (-> string? input?)] [p #,Parser]) #,Parser]{
 produces a @Parser that matches what @racket[p] does, over the tokens
 that @racket[lex] finds from where it's run. It lets a grammar over
 characters use one over tokens for part of its text, or a grammar over
 tokens use another lexer's. What @racket[lex] skips belongs to the
 token after it, so @racket[p] takes what's skipped before its tokens,
 and leaves what follows its match to the grammar around it. Where the
 parse gets furthest inside @racket[p], @racket[parse]'s error says it in
 @racket[lex]'s tokens. Given an @Input that @racket[lex] made,
 @racket[p] simply runs over its tokens.

@examples[#:eval lex-eval
(parse (seq (lit "sum=") (with-lexer sums SUM) (lit ";"))
       "sum=1 + 2;")
(eval:error (parse (seq (lit "sum=") (with-lexer sums SUM))
                   "sum=1 2"))
]}

@defproc[(tokens [lex (-> string? input?)] [str string?]) (listof token?)]{
 produces the tokens that @racket[lex] finds in @racket[str], to show
 what a grammar over them sees. Where no rule matches, it raises
 @racket[exn:fail], saying what's there, as @racket[parse] would.

@examples[#:eval lex-eval
(tokens sums "1 + 2")
]}

@defstruct*[token ([kind symbol?]
                   [value any/c]
                   [start exact-nonnegative-integer?]
                   [end exact-nonnegative-integer?])
            #:transparent]{
 represents a token: its kind, its value, and its start and end
 positions in the text. Parsing doesn't create tokens; only
 @racket[tokens] does.}

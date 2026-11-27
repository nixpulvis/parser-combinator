#lang scribble/manual

@(require "shared.rkt"
          (for-label racket/base
                     racket/contract/base
                     parser-combinator/parse
                     parser-combinator/lex
                     (only-in parser-combinator/json json-lexer)))

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

@deftech{Lexer} : @racket[lexer?]

A @Lexer is made by @racket[lexer]. It is a function: applied to a
 string, it makes an @Input whose tokens a grammar over tokens matches.
 By convention, a lexer is named for the language it lexes, like
 @racket[json-lexer], and parsers are named in capitals, like
 @racket[SUM] below.

@defproc[(lexer? [v any/c]) boolean?]{
 returns @racket[#t] if @racket[v] is a @Lexer, made by @racket[lexer].}

@defform*[((lexer [kind rule] ...)
           (lexer #:skip skip [kind rule] ...))
          #:contracts ([skip (or/c string? #,Parser)]
                       [kind symbol?]
                       [rule #,Parser])]{
 produces a @|Lexer|. Before each token, it skips whatever
 @racket[skip] matches. A string @racket[skip] means
 @racket[(skip _string)], which matches any run of its characters. Any
 other @racket[skip] is a @Parser, so what's skipped can have a grammar
 of its own. Without @racket[skip], nothing is skipped.
 Each token is matched by the first @racket[rule] that matches some
 text. The token's kind is that rule's @racket[kind], a symbol written
 as for @racket[tok], and its value is the rule's @tech{Value}. A rule
 that matches no text is passed over, since a token can't be empty.
 Only @racket[skip] may match nothing.

@examples[#:eval lex-eval
(define sum-lexer
  (lexer #:skip " "
         ['num (pmap string->number (rx #px"\\d+"))]
         ['plus (lit "+")]))
(define SUM (sep-by+ (tok 'num) (tok 'plus)))
(parse SUM (sum-lexer "1 + 2 +3"))
(eval:error (parse SUM (sum-lexer "1 + + 2")))
]

 As in @racket[alt], the rules are tried in order, and the first match
 wins, even if a later rule would match more. So a rule for a keyword,
 placed before the rule for names, splits a name that starts with that
 keyword. To avoid that, lex keywords as names, and match them with
 @racket[tok]'s @racket[value].

 A lexer looks at what its rules and @racket[skip] can begin with when
 it's made. So they're defined before it, with the parsers they use,
 including those made with @racket[define-parser], which it evaluates
 then.}

@defproc*[([(tok [kind symbol?]) #,Parser]
           [(tok [kind symbol?] [value any/c]) #,Parser])]{
 produces a @Parser that matches one token of kind @racket[kind], and
 produces its value. With @racket[value], it only matches a token whose
 value is @racket[equal?] to it, such as one keyword among the names,
 and @racket[parse]'s error expects the kind with that value.
 It needs an @Input made by a lexer. Given a string, it raises
 @racket[exn:fail].

 @racket[alt] chooses among its alternatives by the kind of the next
 token, as it does by the next character. When each alternative begins
 with a different kind of token, only one of them runs.

@examples[#:eval lex-eval
(parse (seq (tok 'num 1) (tok 'plus) (tok 'num))
       (sum-lexer "1+2"))
(eval:error (parse (tok 'num) "1"))
]

 A parser over characters, such as @racket[lit], also works in a grammar
 over tokens, at the same position. It matches exactly the text there,
 so it doesn't skip what the lexer would skip before a token.

@examples[#:eval lex-eval
(parse (seq (tok 'num) (lit "!")) (sum-lexer "5!"))
((seq (tok 'num) (lit "!")) (sum-lexer "5 !"))
]}

@defproc[(with-lexer [lex #,Lexer] [p #,Parser]) #,Parser]{
 produces a @Parser that matches what @racket[p] does, over the tokens
 that @racket[lex] finds from where it runs. This lets a grammar over
 characters use a grammar over tokens for part of its text, or lets a
 grammar over tokens use another lexer. What @racket[lex] skips belongs
 to the token after it, so @racket[p] takes the skipped text before its
 tokens, and leaves anything after its match to the surrounding
 grammar. When the parse gets furthest inside @racket[p],
 @racket[parse]'s error describes it in @racket[lex]'s tokens. Given an
 @Input that @racket[lex] made, @racket[p] runs over its tokens
 directly.

@examples[#:eval lex-eval
(define SUM-SETTING
  (seq (lit "sum=")
       (with-lexer sum-lexer SUM)
       (lit ";")))
(parse SUM-SETTING "sum=1 + 2;")
(eval:error (parse SUM-SETTING "sum=1 2;"))
]}

@defproc[(tokens [lex #,Lexer] [str string?]) (listof token?)]{
 produces the tokens that @racket[lex] finds in @racket[str], to show
 what a grammar over them sees. Where no rule matches, it raises
 @racket[exn:fail], saying what it found and where as @racket[parse]
 does, with what it was lexing if a rule had begun a token.

@examples[#:eval lex-eval
(tokens sum-lexer "1 + 2")
(eval:error (tokens sum-lexer "1 + 2x"))
]}

@defstruct*[token ([kind symbol?]
                   [value any/c]
                   [start exact-nonnegative-integer?]
                   [end exact-nonnegative-integer?])
            #:transparent]{
 represents a token: its kind, its value, and its start and end
 positions in the text. Parsing doesn't create tokens; only
 @racket[tokens] does.}

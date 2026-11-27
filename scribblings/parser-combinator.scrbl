#lang scribble/manual

@(require "shared.rkt"
          (for-label racket/base
                     parser-combinator/parse
                     parser-combinator/lex))

@title{Parser Combinator}

@author{Nathan Lilienthal}

This package provides a small library of @emph{parser combinators}: functions
that consume parsers and produce new parsers. A grammar is written as a
collection of plain definitions, one per non-terminal, each a parser built
from smaller ones.

The package comes with three libraries:
@itemlist[
 @item{@racketmodname[parser-combinator/parse], combinators for building
 parsers}
 @item{@racketmodname[parser-combinator/lex], lexers, for grammars over
 tokens}
 @item{@racketmodname[parser-combinator/json], a JSON parser}
]

@table-of-contents[]

@; -----------------------------------------------------------------------------
@section[#:tag "quick"]{Quick Start}

A @tech{Parser} turns a string into a @tech{Value}. The functions given
 to @racket[pmap] decide what that @tech{Value} is, so a grammar can
 compute the data the program needs while it parses.

Take integers such as @racket["0"], @racket["-7"], and @racket["140"].
 An integer is an optional minus sign followed by either a lone zero or
 a non-zero digit and any number of further digits. @racket[ZERO] and
 @racket[NONZERO] use @racket[pmap] to apply
 @racket[string->number] to the text they match, so the digit parsers
 produce each digit's value instead of its text:

@examples[#:eval quick-eval #:label #f #:no-result
(require parser-combinator/parse)

(code:comment "Parser")
(code:comment "matches the digit 0, producing its value")
(define-parser ZERO
  (pmap string->number (lit "0")))

(code:comment "Parser")
(code:comment "matches a digit 1 through 9, producing its value")
(define-parser NONZERO
  (pmap string->number (rx #rx"[1-9]")))

(code:comment "Parser")
(code:comment "matches any digit, producing its value")
(define-parser DIGIT
  (alt ZERO NONZERO))
]

A natural number other than zero starts with a non-zero digit, and
 @racket[foldl] adds the digits after it from left to right. A missing
 sign is @racket['()], and a minus sign negates the number:

@examples[#:eval quick-eval #:label #f #:no-result
(code:comment "Natural Natural -> Natural")
(code:comment "adds the digit d to the end of n")
(define (add-digit d n)
  (+ (* 10 n) d))

(code:comment "Parser")
(code:comment "matches a natural number, producing its value")
(define-parser NATURAL
  (alt ZERO
       (pmap (lambda (d ds) (foldl add-digit d ds))
             NONZERO
             (star DIGIT))))

(code:comment "Parser")
(code:comment "matches an integer, producing its value")
(define-parser INTEGER
  (pmap (lambda (sign n) (if (null? sign) n (- n)))
        (opt (lit "-"))
        NATURAL))
]

Running a parser with @racket[parse] produces the integer:

@examples[#:eval quick-eval #:label #f
(parse INTEGER "0")
(parse INTEGER "-7")
(parse INTEGER "140")
]

Input that is not in the grammar is an error:

@examples[#:eval quick-eval #:label #f
(eval:error (parse INTEGER "007"))
]

A grammar can also be parsed in two stages, with a
 @seclink["lex"]{lexer} that first splits the text into tokens. A
 lexer's rules are parsers too, so @racket[INTEGER] can be one. This
 lexer skips the spaces between tokens, so the grammar over them never
 mentions them, and @racket[with-lexer] puts the two together into
 @racket[SUM], a parser of text like any other:

@examples[#:eval quick-eval #:label #f #:no-result
(require parser-combinator/lex)

(code:comment "Lexer")
(code:comment "splits a sum into integers and plus signs")
(define sum-lexer
  (lexer #:skip " "
         ['integer INTEGER]
         ['plus (lit "+")]))

(code:comment "Parser")
(code:comment "matches integers joined by plus signs,")
(code:comment "producing their sum")
(define-parser SUM
  (with-lexer sum-lexer
    (pmap (lambda (ns) (apply + ns))
          (sep-by+ (tok 'integer) (tok 'plus)))))
]

@examples[#:eval quick-eval #:label #f
(parse SUM "1 + -7 + 140")
(eval:error (parse SUM "1 + + 2"))
]

The source of @racketmodname[parser-combinator/json] builds its data the
 same way, for a larger grammar.

@include-section["parse.scrbl"]
@include-section["lex.scrbl"]
@include-section["json.scrbl"]

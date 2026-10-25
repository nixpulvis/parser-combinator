#lang scribble/manual

@(require "shared.rkt"
          (for-label racket/base
                     parser-combinator/parse))

@title{Parser Combinator}

@author{Nathan Lilienthal}

This package provides a small library of @emph{parser combinators}: functions
that consume parsers and produce new parsers. Each parser is a Racket function
from a string to a result, so a grammar is written as a collection of plain
definitions, one per non-terminal.

The package comes with two libraries:
@itemlist[
 @item{@racketmodname[parser-combinator/parse], combinators for building
 parsers}
 @item{@racketmodname[parser-combinator/json], a JSON parser}
]

@table-of-contents[]

@; -----------------------------------------------------------------------------
@section[#:tag "quick"]{Quick Start}

A @tech{Parser} turns a string into a @tech{Value}. The functions given
 to @racket[fmap] decide what that @tech{Value} is, so a grammar can
 compute the data the program needs while it parses.

Take numbers such as @racket["0"], @racket["-7"], and @racket["140"]. A
 number is an optional minus sign followed by either a lone zero or a
 non-zero digit and any number of further digits. @racket[ZERO] and
 @racket[NONZERO-DIGIT] use @racket[fmap] to apply
 @racket[string->number] to the text they match, so the digit parsers
 produce each digit's value instead of its text:

@examples[#:eval quick-eval #:label #f #:no-result
(require parser-combinator/parse)

(code:comment "Parser")
(code:comment "matches the digit 0, producing its value")
(define-parser ZERO
  (fmap string->number (lit "0")))

(code:comment "Parser")
(code:comment "matches a digit 1 through 9, producing its value")
(define-parser NONZERO-DIGIT
  (fmap string->number (rx #rx"[1-9]")))

(code:comment "Parser")
(code:comment "matches any digit, producing its value")
(define-parser DIGIT
  (alt ZERO NONZERO-DIGIT))
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
       (fmap (lambda (d ds) (foldl add-digit d ds))
             (seq NONZERO-DIGIT (star DIGIT)))))

(code:comment "Parser")
(code:comment "matches an integer, producing its value")
(define-parser NUMBER
  (fmap (lambda (sign n) (if (null? sign) n (- n)))
        (seq (opt (lit "-")) NATURAL)))
]

Running a parser with @racket[parse] produces the number:

@examples[#:eval quick-eval #:label #f
(parse NUMBER "0")
(parse NUMBER "-7")
(parse NUMBER "140")
]

Input that is not in the grammar is an error:

@examples[#:eval quick-eval #:label #f
(eval:error (parse NUMBER "007"))
]

The source of @racketmodname[parser-combinator/json] builds its data the
 same way, for a larger grammar.

@include-section["parse.scrbl"]
@include-section["json.scrbl"]

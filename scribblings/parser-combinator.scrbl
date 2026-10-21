#lang scribble/manual

@(require "shared.rkt"
          (for-label racket/base
                     parser-combinator/parse))

@title{Parser Combinator}

@author{Nathan Lilienthal}

This package provides a small library of @emph{parser combinators}: functions
that consume parsers and produce new parsers. Each parser is an ordinary Racket
function from a string to a result, so a grammar is written as a collection of
plain definitions, one per non-terminal.

The package comes with two libraries:
@itemlist[
 @item{@racketmodname[parser-combinator/parse], combinators for building
 parsers}
 @item{@racketmodname[parser-combinator/json], a JSON parser}
]

@table-of-contents[]

@; -----------------------------------------------------------------------------
@section[#:tag "quick"]{Quick Start}

Parsing happens in two steps. First a @tech{Parser} turns a string into a
 @tech{Value}, made of strings for the text that matched and lists for
 sequences and repetitions. Then an ordinary recursive function turns the
 @tech{Value} into the data the program needs.

Take numbers such as @racket["0"], @racket["-7"], and @racket["140"]. A
 number is an optional minus sign followed by either a lone zero or a
 non-zero digit and any number of further digits:

@examples[#:eval quick-eval #:label #f #:no-result
(require parser-combinator/parse)

(code:comment "Parser")
(define-parser NONZERO-DIGIT
  (alt* (lit "1") (lit "2") (lit "3") (lit "4") (lit "5")
        (lit "6") (lit "7") (lit "8") (lit "9")))

(code:comment "Parser")
(define-parser DIGIT
  (alt (lit "0") NONZERO-DIGIT))

(code:comment "Parser")
(define-parser NUMBER
  (seq* list
        (opt (lit "-"))
        (alt (lit "0")
             (seq cons NONZERO-DIGIT (star DIGIT)))))
]

Running a parser with @racket[parse] produces the @tech{Value}. A missing
 sign is @racket['()]:

@examples[#:eval quick-eval #:label #f
(parse NUMBER "-7")
(parse NUMBER "140")
]

To get a number out of the @tech{Value}, design a function that follows
 the grammar. Each digit is a string. Unless the number is a lone zero,
 its digits form a list, which @racket[foldl] walks from left to right:

@examples[#:eval quick-eval #:label #f
(code:comment "String -> Natural")
(code:comment "computes the value of the digit d")
(code:line
 (define (digit->number d)
   (- (char->integer (string-ref d 0))
      (char->integer #\0)))
 code:blank)
(code:comment "String Natural -> Natural")
(code:comment "adds the digit d to the end of n")
(code:line
 (define (add-digit d n)
   (+ (* 10 n) (digit->number d)))
 code:blank)
(code:comment "Value -> Natural")
(code:comment "computes the value of the part after the sign")
(code:line
 (define (integer->number v)
   (if (string? v)
       0
       (foldl add-digit 0 v)))
 code:blank)
(code:comment "Value -> Integer")
(code:comment "converts the Value of a number into a number")
(code:line
 (define (value->number v)
   (if (string? (car v))
       (- (integer->number (cadr v)))
       (integer->number (cadr v))))
 code:blank)
(value->number (parse NUMBER "0"))
(value->number (parse NUMBER "-7"))
(value->number (parse NUMBER "140"))
]

Input that is not in the grammar is an error:

@examples[#:eval quick-eval #:label #f
(eval:error (parse NUMBER "007"))
]

The source of @racketmodname[parser-combinator/json] skips the second
 step for a larger grammar: its parsers use @racket[seq] and
 @racket[seq*] constructors to build the data while they parse.

@include-section["parse.scrbl"]
@include-section["json.scrbl"]

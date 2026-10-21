#lang scribble/manual

@(require "shared.rkt"
          (for-label racket/base
                     racket/contract/base
                     parser-combinator/parse
                     parser-combinator/json))

@(define Value @tech{Value})
@(define JSON @tech{JSON})
@(define Parser @tech{Parser})
@(define json-site @link["https://www.json.org"]{JSON web site})

@title[#:tag "json"]{JSON}

@defmodule[parser-combinator/json]

This library parses text in the JSON data exchange format into Racket
 values. See the @|json-site| for more about JSON. The library is written
 entirely with @racketmodname[parser-combinator/parse].

@; -----------------------------------------------------------------------------
@section[#:tag "json-values"]{JSON Values}

@deftech{JSON} : @racket[any/c]

A @JSON is one of:
@itemlist[
 @item{a @racket[string?], for a JSON string}
 @item{an exact @racket[number?], for a JSON number}
 @item{a @racket[boolean?], for @tt{true} and @tt{false}}
 @item{@racket['null], for @tt{null}}
 @item{a @racket[(listof #,JSON)], for a JSON array, an ordered
 collection of values}
 @item{a @racket[(listof json-pair?)], for a JSON object, a collection of
 named values}
]

Note that an empty JSON array and an empty JSON object are both
 represented as @racket['()].

@defstruct*[json-pair ([name string?] [value #,JSON]) #:transparent]{
 represents one named value in a JSON object, as produced by
 @racket[string->json] and @racket[PAIR] for each name and value in the
 object. The pairs of an object appear in the same order as in the
 text.}

@defproc[(string->json [str string?]) #,JSON]{
 parses @racket[str] as a JSON value, ignoring whitespace around it. It
 raises @racket[exn:fail] if @racket[str] is not valid JSON. It is the
 same as @racket[(parse VALUE str)].

@examples[#:eval json-eval
(string->json "[]")
(string->json "-1.5e3")
(string->json "[1, {\"foo\": false}, null]")
(eval:error (string->json "[1, 2"))
]

 Strings are produced as they appear in the text: escape sequences such
 as @tt{\n} are not decoded.

@examples[#:eval json-eval
(string->json "\"a\\nb\"")
]}

@; -----------------------------------------------------------------------------
@section[#:tag "json-grammar"]{Grammar}

The grammar behind @racket[string->json] is made of the parsers below. A
 few small ones match single characters, and their @|Value|s are the
 matched characters as strings. Then there is one parser for each kind
 of JSON value, which uses @racket[seq] and @racket[seq*] constructors
 to produce the @JSON itself. These parsers are exported so that programs
 can use them directly.

Wherever the grammar allows whitespace around a value or a punctuation
 mark, the parsers skip any number of @racket[SPACE]s with
 @racket[unwrap].

@defthing[SPACE #,Parser]{
 matches one whitespace character: a space, a tab, a newline, or a
 carriage return. The @Value is the character as a string.

@examples[#:eval json-eval
((star SPACE) " \t\nx")
]}

@defthing[NONZERO-DIGIT #,Parser]{
 matches one non-zero digit, @tt{1} through @tt{9}. The @Value is the
 digit as a string.

@examples[#:eval json-eval
(parse NONZERO-DIGIT "7")
(eval:error (parse NONZERO-DIGIT "0"))
]}

@defthing[DIGIT #,Parser]{
 matches one digit, @tt{0} through @tt{9}, using @racket[NONZERO-DIGIT]
 for the non-zero ones. The @Value is the digit as a string.

@examples[#:eval json-eval
(parse DIGIT "0")
]}

@defthing[HEXIT #,Parser]{
 matches one hexadecimal digit: a @racket[DIGIT], or a letter from
 @tt{a} through @tt{f} in either case. The @Value is the character as a
 string.

@examples[#:eval json-eval
(parse HEXIT "F")
]}

@defthing[STRING #,Parser]{
 matches a JSON string, including its quotes. Inside the quotes it
 accepts any character other than @tt{\}, @tt{"}, and the control
 characters U+0000 through U+001F. It also accepts the escape sequences
 @tt{\"}, @tt{\\}, @tt{\/}, @tt{\b}, @tt{\f}, @tt{\n}, @tt{\r}, @tt{\t},
 and @tt{\u} followed by four @racket[HEXIT]s.

 It produces the text between the quotes as a string. Escape sequences
 are left as they are.

@examples[#:eval json-eval
(parse STRING "\"hi\"")
(eval:error (parse STRING "\"a\tb\""))
]}

@defthing[NUMBER #,Parser]{
 matches a JSON number: an optional @tt{-}, then either @tt{0} or a
 @racket[NONZERO-DIGIT] followed by any number of @racket[DIGIT]s, then an
 optional fraction, then an optional exponent. The fraction is a @tt{.}
 followed by one or more @racket[DIGIT]s. The exponent is an @tt{e} or
 @tt{E}, an optional @tt{+} or @tt{-}, and one or more @racket[DIGIT]s.

 It produces the number as an exact number.

@examples[#:eval json-eval
(parse NUMBER "7")
(parse NUMBER "-1.5e2")
(eval:error (parse NUMBER "1."))
]}

@defthing[PAIR #,Parser]{
 matches one name and value in a JSON object, such as
 @tt{"name": true}, and produces a @racket[json-pair] of the name, as
 produced by @racket[STRING], and the value, as produced by
 @racket[VALUE].

@examples[#:eval json-eval
(parse PAIR "\"a\": true")
]}

@defthing[OBJECT #,Parser]{
 matches a JSON object, that is, zero or more @racket[PAIR]s separated by
 commas and surrounded by braces. It produces the list of
 @racket[json-pair]s, in order.

@examples[#:eval json-eval
(parse OBJECT "{}")
(parse OBJECT "{\"a\": 1, \"b\": 2}")
]}

@defthing[ARRAY #,Parser]{
 matches a JSON array, that is, zero or more @racket[VALUE]s separated by
 commas and surrounded by brackets. It produces the list of values, in
 order.

@examples[#:eval json-eval
(parse ARRAY "[true, false]")
]}

@defthing[VALUE #,Parser]{
 matches any JSON value along with the whitespace around it. It
 produces what @racket[STRING], @racket[NUMBER], @racket[OBJECT], or
 @racket[ARRAY] produces, or @racket[#t], @racket[#f], or
 @racket[(quote null)] for @tt{true}, @tt{false}, and @tt{null}.

@examples[#:eval json-eval
(parse VALUE "  null ")
(parse VALUE "[1, true]")
]}

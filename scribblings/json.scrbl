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

@defmodule[parser-combinator/json #:packages ("parser-combinator")]

This library parses text in the JSON data exchange format into Racket
 values. See the @|json-site| for more about JSON. The library is written
 entirely with @racketmodname[parser-combinator/parse].

@; -----------------------------------------------------------------------------
@section[#:tag "json-values"]{JSON Values}

@deftech{JSON} : @racket[any/c]

A @JSON is one of:
@itemlist[
 @item{@racket['null], for @tt{null}}
 @item{a @racket[boolean?], for @tt{true} and @tt{false}}
 @item{an exact @racket[number?], for a JSON number}
 @item{a @racket[string?], for a JSON string}
 @item{a @racket[(listof #,JSON)], for a JSON array, an ordered
 collection of values}
 @item{an immutable @racket[(and/c hash-eq? (hash/c symbol? #,JSON))]
 mapping each name in a JSON object to its value}
]

An empty JSON array is @racket['()] and an empty JSON object is
 @racket[#hasheq()], so every kind of JSON value can be told apart with
 @racket[list?], @racket[hash?], and the other predicates above. If a
 name appears more than once in an object, the last value wins. The
 order of the names in the text is not kept.

@examples[#:eval json-eval
(code:comment "{\"a\": [1, 2], \"b\": {}}")
(define obj (string->json "{\"a\": [1, 2], \"b\": {}}"))
(hash-ref obj 'a)
(list-ref (hash-ref obj 'a) 1)
]

@defproc[(string->json [str string?]) #,JSON]{
 parses @racket[str] as a JSON value, ignoring whitespace around it. It
 raises @racket[exn:fail] if @racket[str] is not valid JSON. It is the
 same as @racket[(parse VALUE str)].

@examples[#:eval json-eval
(string->json "[]")
(string->json "-1.5e3")
(code:comment "[1, {\"foo\": false}, null]")
(string->json "[1, {\"foo\": false}, null]")
(eval:error (string->json "[1, 2"))
]

 Escape sequences in strings are decoded, so @tt{\n} becomes a newline
 character. See @racket[STRING].

@examples[#:eval json-eval
(code:comment "\"a\\nb\"")
(string->json "\"a\\nb\"")
]}

@; -----------------------------------------------------------------------------
@section[#:tag "json-grammar"]{Grammar}

The grammar behind @racket[string->json] is made of the parsers below.
 @racket[SPACE] matches whitespace, and its @Value is the matched text.
 The rest produce the @JSON itself, using @racket[seq] constructors:
 @racket[NUMBER], @racket[STRING], @racket[ARRAY], and
 @racket[OBJECT] for those kinds of value, @racket[PAIR] for one name and
 value in an object, and @racket[VALUE] for any JSON value. These parsers
 are exported so that programs can use them directly.

@defthing[VALUE #,Parser]{
 matches any JSON value along with the whitespace around it. It
 produces @racket[(quote null)], @racket[#t], or @racket[#f] for
 @tt{null}, @tt{true}, and @tt{false}, or what @racket[NUMBER],
 @racket[STRING], @racket[ARRAY], or @racket[OBJECT] produces.

@examples[#:eval json-eval
(parse VALUE "  null ")
(parse VALUE "[1, true]")
]}

@defthing[NUMBER #,Parser]{
 matches a JSON number: an optional @tt{-}, then either @tt{0} or a
 non-zero digit followed by any number of digits, then an optional
 fraction, then an optional exponent. The fraction is a @tt{.} followed
 by one or more digits. The exponent is an @tt{e} or @tt{E}, an optional
 @tt{+} or @tt{-}, and one or more digits.

 It produces the number as an exact number.

@examples[#:eval json-eval
(parse NUMBER "7")
(parse NUMBER "-1.5e2")
(eval:error (parse NUMBER "1."))
]}

@defthing[STRING #,Parser]{
 matches a JSON string, including its quotes. Inside the quotes it
 accepts any character other than @tt{\}, @tt{"}, and the control
 characters U+0000 through U+001F. It also accepts the escape sequences
 @tt{\"}, @tt{\\}, @tt{\/}, @tt{\b}, @tt{\f}, @tt{\n}, @tt{\r}, @tt{\t},
 and @tt{\u} followed by four hexadecimal digits, in either case.

 It produces the text between the quotes as a string, with each escape
 sequence replaced by the character it stands for. A @tt{\u} escape
 names a UTF-16 code unit, so a character outside the Basic Multilingual
 Plane is written as a surrogate pair, two @tt{\u} escapes in a row,
 which together produce one character. A surrogate without its other
 half produces the replacement character U+FFFD.

@examples[#:eval json-eval
(code:comment "\"hi\"")
(parse STRING "\"hi\"")
(code:comment "\"a\\tb \\u00e9 \\uD83D\\uDE00\"")
(parse STRING "\"a\\tb \\u00e9 \\uD83D\\uDE00\"")
(code:comment "\"\\uD83D!\"")
(parse STRING "\"\\uD83D!\"")
(code:comment "\"a b\" with a raw tab, which JSON doesn't allow")
(eval:error (parse STRING "\"a\tb\""))
]}

@defthing[ARRAY #,Parser]{
 matches a JSON array, that is, zero or more @racket[VALUE]s separated by
 commas and surrounded by brackets. It produces the list of values, in
 order.

@examples[#:eval json-eval
(parse ARRAY "[true, false]")
]}

@defthing[OBJECT #,Parser]{
 matches a JSON object, that is, zero or more @racket[PAIR]s separated by
 commas and surrounded by braces. It produces an immutable
 @racket[hasheq] from the name of each @racket[PAIR] to its value.

@examples[#:eval json-eval
(parse OBJECT "{}")
(code:comment "{\"a\": 1, \"b\": 2}")
(parse OBJECT "{\"a\": 1, \"b\": 2}")
]}

@defthing[PAIR #,Parser]{
 matches one name and value in a JSON object, such as
 @tt{"name": true}, and produces a pair of the name, as a symbol, and the
 value, as produced by @racket[VALUE].

@examples[#:eval json-eval
(code:comment "\"a\": true")
(parse PAIR "\"a\": true")
]}

Wherever the grammar allows whitespace around a value or a punctuation
 mark, the parsers skip any whitespace there. Unlike @racket[SPACE], they
 don't copy the whitespace they skip.

@defthing[SPACE #,Parser]{
 matches one or more whitespace characters: @racket[#\space],
 @racket[#\tab], @racket[#\newline], and @racket[#\return]. The @Value
 is the matched text.

@examples[#:eval json-eval
((opt SPACE) " \t\nx")
((opt SPACE) "x")
]}

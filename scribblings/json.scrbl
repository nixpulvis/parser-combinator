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
 matched characters as strings. The rest produce the @JSON itself, using
 @racket[seq] and @racket[seq*] constructors: @racket[NUMBER],
 @racket[STRING], @racket[ARRAY], and @racket[OBJECT] for those kinds of
 value, @racket[PAIR] for one name and value in an object, and
 @racket[VALUE] for any JSON value. These parsers are exported so that
 programs can use them directly.

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

@defthing[HEXIT #,Parser]{
 matches one hexadecimal digit: a @racket[DIGIT], or a letter from
 @tt{a} through @tt{f} in either case. The @Value is the character as a
 string.

@examples[#:eval json-eval
(parse HEXIT "F")
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
(parse OBJECT "{\"a\": 1, \"b\": 2}")
]}

@defthing[PAIR #,Parser]{
 matches one name and value in a JSON object, such as
 @tt{"name": true}, and produces a pair of the name, as a symbol, and the
 value, as produced by @racket[VALUE].

@examples[#:eval json-eval
(parse PAIR "\"a\": true")
]}

Wherever the grammar allows whitespace around a value or a punctuation
 mark, the parsers skip any number of @racket[SPACE]s with
 @racket[unwrap].

@defthing[SPACE #,Parser]{
 matches one whitespace character: a space, a tab, a newline, or a
 carriage return. The @Value is the character as a string.

@examples[#:eval json-eval
((star SPACE) " \t\nx")
]}

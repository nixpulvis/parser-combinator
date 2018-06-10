# parser-combinator

Simple Racket parser library, and default (tested) JSON implementation.

## Install

```sh
raco pkg install parser-combinator
```

## Usage

```racket
#lang racket
(require parser-combinator/json)

(string->json "[]")
;=> '()
(string->json "[1, {\"foo\": false}, 2]")
;=> (list 1 (list (json-pair "foo" #f)) 2)
```

## Tests

Run all of the tests.

```sh
raco test *tests.rkt
```

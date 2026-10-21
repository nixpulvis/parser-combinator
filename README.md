# parser-combinator

Simple Racket parser library, and default (tested) JSON implementation.

## Install

```sh
raco pkg install parser-combinator
```

## Parser Usage

```racket
#lang racket
(require parser-combinator/parse)

(define-parser GREETING
  (seq list (lit "hello ") (alt (lit "world") (lit "racket"))))

(parse GREETING "hello world")
;=> '("hello " "world")
(apply string-append (parse GREETING "hello racket"))
;=> "hello racket"
```

The [Quick Start](https://nixpulvis.com/parser-combinator/quick.html) has a
longer example. Otherwise, you can read the JSON parser's implementation as
well.

## JSON Usage

```racket
#lang racket
(require parser-combinator/json)

(string->json "[]")
;=> '()
(string->json "[1, {\"foo\": false}, 2]")
;=> (list 1 (list (json-pair "foo" #f)) 2)
```




## Tests

The tests require `compiler-lib` (provides `raco test`) and `htdp-lib` (provides
`test-engine/racket-tests`). Both ship with the full Racket distribution. On a
minimal install (e.g. Homebrew's `minimal-racket`), install them first.

```sh
raco pkg install --auto --skip-installed compiler-lib htdp-lib
```

Run all of the tests.

```sh
raco test *-tests.rkt
```

## Docs

Building the manual requires `scribble-lib` (provides `raco scribble`) and
`racket-doc` (the Racket docs the manual links into). Both ship with the full
Racket distribution. On a minimal install, install them first.

```sh
raco pkg install --auto --skip-installed scribble-lib racket-doc
```

Build the manual into `_site/`.

```sh
scripts/build-docs.sh
```

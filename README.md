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
(string->json "{}")
;=> '#hasheq()
(string->json "[1, {\"foo\": false}, 2]")
;=> '(1 #hasheq((foo . #f)) 2)
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

## Benchmarks

The benchmarks time `string->json` on generated JSON of a few shapes and sizes.
They require the [`benchmark`](https://pkgs.racket-lang.org/package/benchmark)
package, which isn't a dependency of this package.

```sh
raco pkg install --auto --skip-installed benchmark
```

Benchmark the working tree.

```sh
scripts/bench.rkt
```

Name git refs to compare them instead, each checked out into a temporary
worktree, for example to check a branch for regressions against `master`.
`--merge` also benchmarks a merge of each combination of the refs, to see how
branches perform together before they're merged. Combinations that conflict, or
that merge to the same code as one already benchmarked, are skipped. `--racket`
also benchmarks Racket's own JSON parser, `string->jsexpr`, and shows each time
relative to its. `--plot` also writes a plot of the times, on a log scale so a
much slower ref doesn't flatten the others. See `scripts/bench.rkt --help` for
the other options.

```sh
scripts/bench.rkt --plot bench.png master my-branch
scripts/bench.rkt --merge my-branch other-branch
```

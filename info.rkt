#lang info
(define collection "parser-combinator")
(define deps '("base"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"
                     "scribble-lib"))
(define scribblings '(("scribblings/parser-combinator.scrbl" (multi-page))))
; scripts/bench.rkt needs the benchmark package, which the library does
; not, so raco setup and raco test leave scripts/ alone.
(define compile-omit-paths '("scripts"))
(define test-omit-paths '("scripts"))
(define pkg-desc "Parser combinators and a JSON parser.")
(define version "1.0.1")
(define pkg-authors '(nixpulvis))

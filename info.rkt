#lang info
(define collection "parser-combinator")
(define deps '("htdp-lib"
               "base"))
(define build-deps '("racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))
(define scribblings '(("scribblings/parser-combinator.scrbl" (multi-page))))
(define pkg-desc "Parser combinators and a JSON parser.")
(define version "1.0.1")
(define pkg-authors '(nixpulvis))

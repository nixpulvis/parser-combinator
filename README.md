# parser-combinator

Simple Racket parser library, and default (tested) JSON implementation.

## Install

```sh
raco pkg install parser-combinator
```

## JSON Usage

```racket
#lang racket
(require parser-combinator/json)

(string->json "[]")
;=> '()
(string->json "[1, {\"foo\": false}, 2]")
;=> (list 1 (list (json-pair "foo" #f)) 2)
```

## Parser Usage

```racket
#lang racket
(require parser-combinator/parse)

(define-parser DIGIT+ (alt* (lit "0")
                            (lit "1")
                            (lit "2")
                            (lit "3")
                            (lit "4")
                            (lit "5")
                            (lit "6")
                            (lit "7")
                            (lit "8")
                            (lit "9")))

(define-parser DIGIT (alt* (lit "0") DIGIT+))

(define-parser NUMBER (seq* 'number
                           (opt (lit "-"))
                           (alt* (lit "0") (seq* 'significand
                                                 DIGIT+
                                                 (star DIGIT)))))

; (seq-node 'number AST AST) -> Number
(define (parse-number ast)
  (string->number (string-append "#e" (flatten-ast ast))))

; String -> Number
(define (parse str)
  (parse-number (parse NUMBER str)))

(println (parse "0"))
(println (parse "-0"))
(println (parse "1"))
(println (parse "140"))
```

## Tests

Run all of the tests.

```sh
raco test *tests.rkt
```

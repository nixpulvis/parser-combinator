#lang racket
(require test-engine/racket-tests)
(require "parse.rkt")
(require (submod "parse.rkt" positions))

; define-parser tests
(define builds 0)
(define-parser COUNTED
  (begin (set! builds (add1 builds))
         (lit "x")))
(check-expect builds 0)
(check-expect (list (COUNTED "xy") (COUNTED "x") (COUNTED "y") builds)
              (list (result "x" "y") (result "x" "") 'error 1))

; lit tests
(check-expect ((lit "-") "-1.35")
              (result "-" "1.35"))
(check-expect ((lit "-") "-")
              (result "-" ""))
(check-expect ((lit "hi") "high")
              (result "hi" "gh"))
(check-expect ((lit "") "")
              (result "" ""))
(check-expect ((lit "") "walrus")
              (result "" "walrus"))
(check-expect ((lit "-") "hi")
              'error)
(check-expect ((lit "high") "hi")
              'error)
(check-expect ((lit "high") "hippo")
              'error)

; rx tests
(check-expect ((rx #rx"[a-c]+") "abcd")
              (result "abc" "d"))
(check-expect ((rx #rx"[a-c]+") "dabc")
              'error)
(check-expect ((rx #rx"a|b") "xb")
              'error)
(check-expect ((rx #px"\\d+") "42x")
              (result "42" "x"))
(check-expect ((rx #rx"a") "")
              'error)

; alt tests
(check-expect ((alt (lit "+") (lit "-")) "+123")
              (result "+" "123"))
(check-expect ((alt (lit "+") (lit "-")) "-123")
              (result "-" "123"))
(check-expect ((alt (lit "+") (alt (lit "-") (lit "."))) ".foo")
              (result "." "foo"))
(check-expect ((alt (lit "hi") (lit "high")) "higher")
              (result "hi" "gher"))
(check-expect ((alt (lit "+") (lit "-")) "007")
              'error)

; alt* tests
(check-expect ((alt* (lit "+") (lit "-") (lit ".")) "-123")
              (result "-" "123"))
(check-expect ((alt* (lit "+") (lit "-") (lit ".")) "-+.")
              (result "-" "+."))
(check-expect ((alt* (lit "1") (lit "12") (lit "123")) "123")
              (result "1" "23"))
(check-expect ((alt* (lit "+") (lit "-") (lit ".")) "123")
              'error)

; seq tests
(check-expect ((seq list (lit "-") (lit "+")) "-+...")
              (result (list "-" "+") "..."))
(check-expect ((seq string-append (lit "-") (lit "+")) "-+...")
              (result "-+" "..."))
(check-expect ((seq list (lit "-") (lit "+")) "+-...")
              'error)
(check-expect ((seq list (lit "-") (lit "+")) "--...")
              'error)
(define (one a) a)
(define (two a b) a)
(check-error (seq one (lit "-") (lit "+"))
             (string-append "seq: contract violation\n"
                            "  expected: (procedure-arity-includes/c 2)\n"
                            "  given: #<procedure:one>"))
(check-error (seq 'a (lit "-") (lit "+"))
             (string-append "seq: contract violation\n"
                            "  expected: (procedure-arity-includes/c 2)\n"
                            "  given: 'a"))

; seq* tests
(check-expect ((seq* list (lit "-")) "-+")
              (result (list "-") "+"))
(check-expect ((seq* list (lit "-") (lit "+") (lit ".")) "-+...")
              (result (list "-" "+" ".") ".."))
(check-expect ((seq* (lambda (l m r) m) (lit "(") (lit "x") (lit ")")) "(x)!")
              (result "x" "!"))
(check-expect ((seq* list (lit "-") (lit "+")) "+-...")
              'error)
(check-expect ((seq* list (lit "-") (lit "+") (lit ".")) "-+-")
              'error)
(check-error (seq* two (lit "-") (lit "+") (lit "."))
             (string-append "seq*: contract violation\n"
                            "  expected: (procedure-arity-includes/c 3)\n"
                            "  given: #<procedure:two>"))
(check-error (seq* 'a (lit "-") (lit "+"))
             (string-append "seq*: contract violation\n"
                            "  expected: (procedure-arity-includes/c 2)\n"
                            "  given: 'a"))

; opt tests
(check-expect ((opt (lit "-")) "-123")
              (result "-" "123"))
(check-expect ((opt (lit "-")) "123")
              (result '() "123"))
(check-expect ((opt (lit "-")) "")
              (result '() ""))

; where tests
(define NUMBER (seq* string->number (rx #px"\\d+")))
(check-expect ((where NUMBER even?) "42!")
              (result 42 "!"))
(check-expect ((where NUMBER even?) "41!")
              'error)
(check-expect ((where NUMBER even?) "x")
              'error)
(check-expect ((seq list (lit "#") (where NUMBER even?)) "#42!")
              (result (list "#" 42) "!"))
(check-expect ((seq list (lit "#") (where NUMBER even?)) "#41!")
              'error)
(check-expect ((alt (where (lit "ab") (const #f)) (lit "a")) "abc")
              (result "a" "bc"))

; emp tests
(check-expect ((emp) "123")
              (result '() "123"))
(check-expect ((emp) "")
              (result '() ""))

; plus tests
(check-expect ((plus (lit "+")) "123")
              'error)
(check-expect ((plus (lit "+")) "")
              'error)
(check-expect ((plus (lit "+")) "+123")
              (result (list "+") "123"))
(check-expect ((plus (lit "+")) "++123")
              (result (list "+" "+") "123"))

; star tests
(check-expect ((star (lit "+")) "123")
              (result '() "123"))
(check-expect ((star (lit "+")) "+123")
              (result (list "+") "123"))
(check-expect ((star (lit "+")) "++123")
              (result (list "+" "+") "123"))
(check-expect ((star (alt (lit "1") (lit "2"))) "123")
              (result (list "1" "2") "3"))
(check-expect ((star (alt (lit "1") (lit "2"))) "12")
              (result (list "1" "2") ""))
(check-expect ((star (seq list (lit "a") (lit "b"))) "abab")
              (result (list (list "a" "b") (list "a" "b")) ""))

; unwrap tests
(check-expect ((unwrap (lit "+") (lit "!") (lit "+")) "+!+")
              (result "!" ""))
(check-expect ((unwrap (lit "+") (lit "!") (lit "+")) "+!+++")
              (result "!" "++"))
(check-expect ((unwrap (lit "+") (star (lit "!")) (lit "+")) "+!!+++")
              (result (list "!" "!") "++"))
(check-expect ((unwrap (lit "+") (lit "!") (lit "+")) "+!-")
              'error)

; plain function parsers
; A plain function is given the rest of the input, and its `remaining`
; tells the combinators where its match ended.
(check-expect ((seq list (lit "ab") (lambda (s) (result s (substring s 1)))) "abcd")
              (result (list "ab" "cd") "d"))
(check-expect ((seq list (lit "a") (lambda (s) 'error)) "ab")
              'error)

; positions tests
; Char-Predicate -> Parser
; matches one character that `ok?` accepts
(define (char-where ok?)
  (parser
   (lambda (s i)
     (if (and (< i (string-length s)) (ok? (string-ref s i)))
         (matched (string (string-ref s i)) (add1 i))
         'error))))
(check-expect ((char-where char-alphabetic?) "ab")
              (result "a" "b"))
(check-expect ((char-where char-alphabetic?) "1")
              'error)
(check-expect ((seq list (lit "1") (star (char-where char-alphabetic?))) "1ab2")
              (result (list "1" (list "a" "b")) "2"))
(check-expect (run (lit "b") "ab" 1)
              (matched "b" 2))
(check-expect (run (lit "b") "ab" 0)
              'error)
(check-expect (run (lambda (s) (result s (substring s 1))) "abc" 1)
              (matched "bc" 2))
; A parser made with `parser` is given the input itself, not a copy.
(define seen #f)
(define spy
  (parser (lambda (s i)
            (set! seen (cons s i))
            (matched '() i))))
(define spied "ab!")
(check-expect (begin ((seq list (lit "ab") spy) spied)
                     (and (eq? (car seen) spied) (cdr seen)))
              2)

; parse tests
(check-expect (parse (emp) "") '())
(check-error (parse (lit "a") "b") "bad input, got error")
(check-error (parse (emp) "{}") "bad input, had leftover text \"{}\"")

(test)

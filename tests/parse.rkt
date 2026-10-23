#lang racket
(require rackunit)
(require "../parse.rkt")
(require (submod "../parse.rkt" positions))

; String ... -> [Exn -> Boolean]
; Whether an exception's message is the `parts` joined together.
(define ((message-is . parts) e)
  (equal? (exn-message e) (apply string-append parts)))

; define-parser tests
(define builds 0)
(define-parser COUNTED
  (begin (set! builds (add1 builds))
         (lit "x")))
(check-equal? builds 0)
(check-equal? (list (COUNTED "xy") (COUNTED "x") (COUNTED "y") builds)
              (list (result "x" "y") (result "x" "") 'error 1))

; lit tests
(check-equal? ((lit "-") "-1.35")
              (result "-" "1.35"))
(check-equal? ((lit "-") "-")
              (result "-" ""))
(check-equal? ((lit "hi") "high")
              (result "hi" "gh"))
(check-equal? ((lit "") "")
              (result "" ""))
(check-equal? ((lit "") "walrus")
              (result "" "walrus"))
(check-equal? ((lit "-") "hi")
              'error)
(check-equal? ((lit "high") "hi")
              'error)
(check-equal? ((lit "high") "hippo")
              'error)

; rx tests
(check-equal? ((rx #rx"[a-c]+") "abcd")
              (result "abc" "d"))
(check-equal? ((rx #rx"[a-c]+") "dabc")
              'error)
(check-equal? ((rx #rx"a|b") "xb")
              'error)
(check-equal? ((rx #px"\\d+") "42x")
              (result "42" "x"))
(check-equal? ((rx #rx"a") "")
              'error)

; span tests
(check-equal? ((span char-alphabetic?) "abc123")
              (result "abc" "123"))
(check-equal? ((span char-alphabetic?) "abc")
              (result "abc" ""))
(check-equal? ((span char-numeric?) "abc")
              'error)
(check-equal? ((span char-numeric?) "")
              'error)
(check-equal? ((seq (lit "x") (span char-numeric?)) "x42y")
              (result (list "x" "42") "y"))

; alt tests
(check-equal? ((alt (lit "+") (lit "-")) "+123")
              (result "+" "123"))
(check-equal? ((alt (lit "+") (lit "-")) "-123")
              (result "-" "123"))
(check-equal? ((alt (lit "+") (alt (lit "-") (lit "."))) ".foo")
              (result "." "foo"))
(check-equal? ((alt (lit "hi") (lit "high")) "higher")
              (result "hi" "gher"))
(check-equal? ((alt (lit "+") (lit "-")) "007")
              'error)

; alt* tests
(check-equal? ((alt* (lit "+") (lit "-") (lit ".")) "-123")
              (result "-" "123"))
(check-equal? ((alt* (lit "+") (lit "-") (lit ".")) "-+.")
              (result "-" "+."))
(check-equal? ((alt* (lit "1") (lit "12") (lit "123")) "123")
              (result "1" "23"))
(check-equal? ((alt* (lit "+") (lit "-") (lit ".")) "123")
              'error)

; switch tests
(define PICK
  (switch [(#\a #\b) (rx #rx"[ab]+")]
          [(#\1) (lit "12")]
          [else (opt (rx #rx".+"))]))
(check-equal? (PICK "abba!")
              (result "abba" "!"))
(check-equal? (PICK "baa")
              (result "baa" ""))
(check-equal? (PICK "12x")
              (result "12" "x"))
; The chosen clause failing fails switch, even though `else` would match.
(check-equal? (PICK "13")
              'error)
(check-equal? (PICK "xyz")
              (result "xyz" ""))
(check-equal? (PICK "")
              (result '() ""))
(check-equal? ((seq (lit "(") PICK) "(ab)")
              (result (list "(" "ab") ")"))
(check-equal? ((switch [(#\a) (lit "a")]) "b")
              'error)
(check-equal? ((switch [(#\a) (lit "a")]) "")
              'error)
; Clause parsers are built once, when switch is, not on every run.
(define switch-builds 0)
(define (counted-lit str)
  (set! switch-builds (add1 switch-builds))
  (lit str))
(define COUNTED-SWITCH
  (switch [(#\a) (counted-lit "a")]
          [else (counted-lit "b")]))
(check-equal? (list (COUNTED-SWITCH "a") (COUNTED-SWITCH "b") (COUNTED-SWITCH "")
                    switch-builds)
              (list (result "a" "") (result "b" "") 'error 2))
; A malformed clause is a syntax error, rather than expanding forever.
(define-namespace-anchor anchor)
(define (expand-switch clauses)
  (parameterize ([current-namespace (namespace-anchor->namespace anchor)])
    (expand `(switch ,@clauses))))
(check-exn (message-is "switch: bad syntax\n"
                       "  in: (switch (#\\a (lit \"a\")))")
           (λ () (expand-switch '([#\a (lit "a")]))))
(check-exn (message-is "switch: bad syntax\n"
                       "  in: (switch (else (lit \"b\")) ((#\\a) (lit \"a\")))")
           (λ () (expand-switch '([else (lit "b")] [(#\a) (lit "a")]))))

; seq tests
(check-equal? ((seq (lit "-") (lit "+")) "-+...")
              (result (list "-" "+") "..."))
(check-equal? ((seq (lit "-")) "-+")
              (result (list "-") "+"))
(check-equal? ((seq (lit "-") (lit "+")) "+-...")
              'error)
(check-equal? ((seq (lit "-") (lit "+")) "--...")
              'error)
; Every length of sequence, from one parser to more than `all-of` writes
; a Run out for, matches the same way.
(define LETTERS (string->list "abcdefghijklmnopqrst"))
(for ([n (in-range 1 (add1 (length LETTERS)))])
  (let* ([cs (take LETTERS n)]
         [ps (for/list ([c (in-list cs)]) (lit (string c)))]
         [text (list->string cs)])
    (check-equal? ((apply seq ps) (string-append text "!"))
                  (result (map string cs) "!"))
    (check-equal? ((apply pmap string-append ps) text)
                  (result text ""))
    (check-equal? ((apply seq ps) (string-append (substring text 0 (sub1 n)) "?"))
                  'error)))
; A seq's Value is one Value, like any other parser's.
(define AB (seq (lit "a") (lit "b")))
(check-equal? ((seq AB (lit "c")) "abc")
              (result (list (list "a" "b") "c") ""))

; pmap tests
; With one parser, the function converts its Value.
(check-equal? ((pmap string->number (rx #px"\\d+")) "42!")
              (result 42 "!"))
(check-equal? ((pmap string->number (rx #px"\\d+")) "x")
              'error)
(check-equal? ((pmap length (star (lit "a"))) "aab")
              (result 2 "b"))
(check-equal? ((pmap identity AB) "abc")
              (result (list "a" "b") "c"))
; With more, it runs them in order, and gets one Value per parser.
(check-equal? ((pmap string-append (lit "-") (lit "+")) "-+...")
              (result "-+" "..."))
(check-equal? ((pmap (lambda (l m r) m) (lit "(") (lit "x") (lit ")")) "(x)!")
              (result "x" "!"))
(check-equal? ((pmap string-append (lit "-") (lit "+")) "+-...")
              'error)
; The function's arity is checked when the parser is made.
(define (one a) a)
(define (two a b) a)
(check-exn (message-is "pmap: contract violation\n"
                       "  expected: (procedure-arity-includes/c 1)\n"
                       "  given: #<procedure:two>")
           (λ () (pmap two (lit "-"))))
(check-exn (message-is "pmap: contract violation\n"
                       "  expected: (procedure-arity-includes/c 1)\n"
                       "  given: 'a")
           (λ () (pmap 'a (lit "-"))))
(check-exn (message-is "pmap: contract violation\n"
                       "  expected: (procedure-arity-includes/c 3)\n"
                       "  given: #<procedure:two>")
           (λ () (pmap two (lit "-") (lit "+") (lit "."))))
(check-exn (message-is "pmap: contract violation\n"
                       "  expected: (procedure-arity-includes/c 2)\n"
                       "  given: #<procedure:one>")
           (λ () (pmap one (lit "-") (lit "+"))))

; opt tests
(check-equal? ((opt (lit "-")) "-123")
              (result "-" "123"))
(check-equal? ((opt (lit "-")) "123")
              (result '() "123"))
(check-equal? ((opt (lit "-")) "")
              (result '() ""))
; With a default, a failed match produces it, and a match ignores it.
(check-equal? ((opt (lit "-") #f) "123")
              (result #f "123"))
(check-equal? ((opt (lit "-") #f) "-123")
              (result "-" "123"))

; where tests
(define NUMBER (pmap string->number (rx #px"\\d+")))
(check-equal? ((where even? NUMBER) "42!")
              (result 42 "!"))
(check-equal? ((where even? NUMBER) "41!")
              'error)
(check-equal? ((where even? NUMBER) "x")
              'error)
(check-equal? ((seq (lit "#") (where even? NUMBER)) "#42!")
              (result (list "#" 42) "!"))
(check-equal? ((seq (lit "#") (where even? NUMBER)) "#41!")
              'error)
(check-equal? ((alt (where (const #f) (lit "ab")) (lit "a")) "abc")
              (result "a" "bc"))

; emp tests
(check-equal? ((emp) "123")
              (result '() "123"))
(check-equal? ((emp) "")
              (result '() ""))
(check-equal? ((emp 0) "123")
              (result 0 "123"))

; plus tests
(check-equal? ((plus (lit "+")) "123")
              'error)
(check-equal? ((plus (lit "+")) "")
              'error)
(check-equal? ((plus (lit "+")) "+123")
              (result (list "+") "123"))
(check-equal? ((plus (lit "+")) "++123")
              (result (list "+" "+") "123"))

; star tests
(check-equal? ((star (lit "+")) "123")
              (result '() "123"))
(check-equal? ((star (lit "+")) "+123")
              (result (list "+") "123"))
(check-equal? ((star (lit "+")) "++123")
              (result (list "+" "+") "123"))
(check-equal? ((star (alt (lit "1") (lit "2"))) "123")
              (result (list "1" "2") "3"))
(check-equal? ((star (alt (lit "1") (lit "2"))) "12")
              (result (list "1" "2") ""))
(check-equal? ((star (seq (lit "a") (lit "b"))) "abab")
              (result (list (list "a" "b") (list "a" "b")) ""))
; star stops at a match that consumes nothing, rather than looping.
(check-equal? ((star (opt (lit "a"))) "aab")
              (result (list "a" "a") "b"))
(check-equal? ((star (rx #rx"a*")) "aab")
              (result (list "aa") "b"))
(check-equal? ((plus (opt (lit "a"))) "b")
              (result (list '()) "b"))

; unwrap tests
(check-equal? ((unwrap (lit "+") (lit "!") (lit "+")) "+!+")
              (result "!" ""))
(check-equal? ((unwrap (lit "+") (lit "!") (lit "+")) "+!+++")
              (result "!" "++"))
(check-equal? ((unwrap (lit "+") (star (lit "!")) (lit "+")) "+!!+++")
              (result (list "!" "!") "++"))
(check-equal? ((unwrap (lit "+") (lit "!") (lit "+")) "+!-")
              'error)

; preceded and terminated tests
(check-equal? ((preceded (lit "$") (lit "4")) "$42")
              (result "4" "2"))
(check-equal? ((preceded (lit "$") (lit "4")) "4")
              'error)
(check-equal? ((preceded (lit "$") (lit "4")) "$5")
              'error)
(check-equal? ((terminated (lit "4") (lit "%")) "4%2")
              (result "4" "2"))
(check-equal? ((terminated (lit "4") (lit "%")) "4")
              'error)
(check-equal? ((terminated (lit "4") (lit "%")) "%")
              'error)

; plain function parsers
; A plain function is given the rest of the input, and its `remaining`
; tells the combinators where its match ended.
(check-equal? ((seq (lit "ab") (lambda (s) (result s (substring s 1)))) "abcd")
              (result (list "ab" "cd") "d"))
(check-equal? ((seq (lit "a") (lambda (s) 'error)) "ab")
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
(check-equal? ((char-where char-alphabetic?) "ab")
              (result "a" "b"))
(check-equal? ((char-where char-alphabetic?) "1")
              'error)
(check-equal? ((seq (lit "1") (star (char-where char-alphabetic?))) "1ab2")
              (result (list "1" (list "a" "b")) "2"))
(check-equal? (run (lit "b") "ab" 1)
              (matched "b" 2))
(check-equal? (run (lit "b") "ab" 0)
              'error)
(check-equal? (run (lambda (s) (result s (substring s 1))) "abc" 1)
              (matched "bc" 2))
; A parser made with `parser` is given the input itself, not a copy.
(define seen #f)
(define spy
  (parser (lambda (s i)
            (set! seen (cons s i))
            (matched '() i))))
(define spied "ab!")
(check-equal? (begin ((seq (lit "ab") spy) spied)
                     (and (eq? (car seen) spied) (cdr seen)))
              2)

; parse tests
(check-equal? (parse (emp) "") '())
(check-exn (message-is "bad input, got error")
           (λ () (parse (lit "a") "b")))
(check-exn (message-is "bad input, had leftover text \"{}\"")
           (λ () (parse (emp) "{}")))

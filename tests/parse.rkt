#lang racket
(require rackunit)
(require "../parse.rkt")
; The library's private module has everything the positions module does,
; and the Starts, which the tests check.
(require (submod "../parse.rkt" private))
; The positions module's `parser` takes a name where the private one's
; takes a Start, and its `run` takes a String too.
(require (prefix-in positions: (only-in (submod "../parse.rkt" positions)
                                        parser
                                        parser-run
                                        run)))

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
(check-equal? (object-name COUNTED) 'COUNTED)
(check-equal? (format "~a" COUNTED) "#<procedure:COUNTED>")

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
; Given a String, it matches the characters in it, outside ASCII too.
(check-equal? ((span "01") "0110 2")
              (result "0110" " 2"))
(check-equal? ((span "01") "2")
              'error)
(check-equal? ((span "aé") "aéab!")
              (result "aéa" "b!"))
(check-equal? ((seq (lit "x") (span char-numeric?)) "x42y")
              (result (list "x" "42") "y"))

; skip tests
(check-equal? ((skip " \t") " \t x")
              (result '() "x"))
(check-equal? ((skip " \t") "x")
              (result '() "x"))
(check-equal? ((skip " \t") "")
              (result '() ""))
(check-equal? ((skip char-numeric?) "42x")
              (result '() "x"))
(check-equal? ((seq (lit "[") (skip " ") (lit "]")) "[  ]")
              (result (list "[" '() "]") ""))
; skip matches what an optional span does, with '() for its Value.
(for* ([ok? (list " \t" "aé" char-numeric?)]
       [str (list "" " " " \t x" "x \t" "aéab!" "42x" "x42" "é")])
  (check-equal? ((skip ok?) str)
                ((pmap (const '()) (opt (span ok?))) str)
                (format "skip ~s on ~s" ok? str)))

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

(check-equal? ((alt (lit "+") (lit "-") (lit ".")) "-123")
              (result "-" "123"))
(check-equal? ((alt (lit "+") (lit "-") (lit ".")) "-+.")
              (result "-" "+."))
(check-equal? ((alt (lit "1") (lit "12") (lit "123")) "123")
              (result "1" "23"))
(check-equal? ((alt (lit "+") (lit "-") (lit ".")) "123")
              'error)
(check-equal? ((alt) "a")
              'error)
; Alternatives that begin with the same character are tried in order.
(check-equal? ((alt (lit "ab") (lit "ac")) "acd")
              (result "ac" "d"))
; One whose Start isn't known is tried for any character, in its place.
(check-equal? ((alt (rx #rx"[0-9]+") (lit "a")) "a")
              (result "a" ""))
(check-equal? ((alt (lit "1") (rx #rx"[0-9]+")) "12")
              (result "1" "2"))
(check-equal? ((alt (rx #rx"[0-9]+") (lit "1")) "12")
              (result "12" ""))
; One that can match nothing is tried for any character, and at the end.
(check-equal? ((alt (lit "a") (emp)) "b")
              (result '() "b"))
(check-equal? ((alt (lit "a") (opt (lit "b"))) "")
              (result '() ""))
(check-equal? ((alt (lit "a") (lit "b")) "")
              'error)
; Only the alternatives that could match the next character are run.
(define tried '())
(define (spy-for name c)
  (parser (lambda (in i)
            (set! tried (cons name tried))
            (define s (input-text in))
            (if (and (< i (string-length s)) (char=? (string-ref s i) c))
                (matched name (add1 i))
                'error))
          (start (list c) #f)))
(define SPIED (alt (spy-for 'a #\a) (spy-for 'b #\b) (spy-for 'c #\c)))
(check-equal? (list (SPIED "c") (SPIED "b") (SPIED "x") tried)
              (list (result 'c "") (result 'b "") 'error '(b c)))
; alt matches what trying each alternative in order does, whichever
; ones it skips. `in-order` has a Run written by hand, so it tries them
; all.
(define (in-order . ps)
  (parser (lambda (in i)
            (let loop ([ps ps])
              (if (null? ps)
                  'error
                  (let ([m (run (car ps) in i)])
                    (if (symbol? m) (loop (cdr ps)) m)))))))
(for* ([ps (list (list (lit "a") (lit "ab") (lit "b"))
                 (list (lit "ab") (lit "a") (emp 0))
                 (list (opt (lit "x")) (lit "y"))
                 (list (rx #rx"[a-c]+") (lit "a") (span "cd"))
                 (list (seq (opt (lit "-")) (span "01")) (lit "-") (skip " "))
                 (list (star (lit "a")) (lit "b"))
                 (list (lit "é") (span "aé") (span char-numeric?))
                 (list (alt (lit "a") (lit "b")) (alt (lit "b") (lit "c")))
                 (list))]
       [str (list "" "a" "ab" "abc" "b" "c" "d" "x" "y" "-" "-01" "01"
                  " " " a" "é" "éa" "aé" "7" "zz")])
  (check-equal? ((apply alt ps) str)
                ((apply in-order ps) str)
                (format "alt on ~s" str)))
; A grammar that refers to itself.
(define-parser PARENS
  (alt (pmap (lambda (l inside r) (add1 inside))
             (lit "(") PARENS (lit ")"))
       (pmap (const 0) (emp))))
(check-equal? (PARENS "((()))!")
              (result 3 "!"))

; start tests
; Parser -> [Maybe (list [Listof Char] Boolean)]
(define (chars-of p)
  (let ([st (start-of p)])
    (and st (list (sort (remove-duplicates (start-chars st)) char<?)
                  (start-empty? st)))))
(check-equal? (chars-of (lit "ab")) (list (list #\a) #f))
(check-equal? (chars-of (lit "")) (list '() #t))
(check-equal? (chars-of (span "01")) (list (list #\0 #\1) #f))
(check-equal? (chars-of (span char-numeric?)) #f)
(check-equal? (chars-of (skip " \t")) (list (list #\tab #\space) #t))
(check-equal? (chars-of (skip char-numeric?)) #f)
; A parser with a Run written by hand has no Start, and one after a
; `lit` doesn't need one.
(check-equal? (chars-of (parser (lambda (in i) 'error))) #f)
(check-equal? (chars-of (preceded (lit "u") (parser (lambda (in i) 'error))))
              (list (list #\u) #f))
(check-equal? (chars-of (rx #rx"a")) #f)
(check-equal? (chars-of (seq (opt (lit "-")) (lit "1") (lit "2")))
              (list (list #\- #\1) #f))
(check-equal? (chars-of (seq (opt (lit "-")) (star (lit "1"))))
              (list (list #\- #\1) #t))
; star and plus keep their parser's Start even when it can match empty.
(check-equal? (chars-of (star (opt (lit "a")))) (list (list #\a) #t))
(check-equal? (chars-of (plus (opt (lit "a")))) (list (list #\a) #t))
(check-equal? (chars-of (alt (lit "a") (pmap string->symbol (lit "b"))))
              (list (list #\a #\b) #f))
(check-equal? (chars-of (alt (lit "a") (rx #rx"b"))) #f)
(check-equal? (chars-of (where even? (pmap string->number (lit "2"))))
              (list (list #\2) #f))
(check-equal? (chars-of PARENS) (list (list #\() #t))
; A Start that refers to itself, here through a grammar that can loop
; back to itself without consuming anything, is #f rather than an error.
(define-parser LOOPS
  (alt (pmap list LOOPS (lit "x")) (lit "y")))
(check-equal? (chars-of LOOPS) #f)

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
; star matches what its definition by recursion does: a `p` that
; consumes something and then the rest, or else nothing.
(define (star-by-recursion p)
  (letrec ([consuming (parser
                       (lambda (in i)
                         (let ([m (run p in i)])
                           (if (and (matched? m) (> (matched-end m) i))
                               m
                               'error))))]
           [self (alt (pmap cons
                            consuming
                            (parser (lambda (in i) (run self in i))))
                      (emp))])
    self))
(for* ([p (list (lit "a")
                (alt (lit "a") (lit "bb"))
                (opt (lit "a"))
                (rx #rx"a*")
                (seq (lit "a") (opt (lit "b"))))]
       [str (list "" "a" "aaa" "aab" "abab" "aba" "abba" "b" "bbbb")])
  (check-equal? ((star p) str)
                ((star-by-recursion p) str)
                (format "star on ~s" str)))

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

; sep-by* and sep-by+ tests
(check-equal? ((sep-by* (lit "a") (lit ",")) "a,a,ab")
              (result (list "a" "a" "a") "b"))
(check-equal? ((sep-by* (lit "a") (lit ",")) "b")
              (result '() "b"))
(check-equal? ((sep-by* (lit "a") (lit ",")) "")
              (result '() ""))
; A separator with no `p` after it is left in the input.
(check-equal? ((sep-by* (lit "a") (lit ",")) "a,a,")
              (result (list "a" "a") ","))
(check-equal? ((sep-by* (lit "a") (lit ",")) ",a")
              (result '() ",a"))
(check-equal? ((sep-by+ (lit "a") (lit ",")) "a,a")
              (result (list "a" "a") ""))
(check-equal? ((sep-by+ (lit "a") (lit ",")) "a")
              (result (list "a") ""))
(check-equal? ((sep-by+ (lit "a") (lit ",")) "b")
              'error)
(check-equal? ((sep-by+ (lit "a") (lit ",")) "")
              'error)
; sep-by* is opt of sep-by+, and sep-by+ is p then star of sep and p.
(for ([s (in-list '("" "a" "a,a" "a,a," ",a" "a;a" "aa,a"))])
  (check-equal? ((sep-by* (lit "a") (lit ",")) s)
                ((opt (sep-by+ (lit "a") (lit ","))) s))
  (check-equal? ((sep-by+ (lit "a") (lit ",")) s)
                ((pmap cons (lit "a") (star (preceded (lit ",") (lit "a")))) s)))

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
   (lambda (in i)
     (define s (input-text in))
     (if (and (< i (string-length s)) (ok? (string-ref s i)))
         (matched (string (string-ref s i)) (add1 i))
         'error))))
(check-equal? ((char-where char-alphabetic?) "ab")
              (result "a" "b"))
(check-equal? ((char-where char-alphabetic?) "1")
              'error)
(check-equal? ((seq (lit "1") (star (char-where char-alphabetic?))) "1ab2")
              (result (list "1" (list "a" "b")) "2"))
(check-equal? (positions:run (lit "b") "ab" 1)
              (matched "b" 2))
(check-equal? (positions:run (lit "b") "ab" 0)
              'error)
(check-equal? (positions:run (lambda (s) (result s (substring s 1))) "abc" 1)
              (matched "bc" 2))
(check-exn exn:fail:contract? (λ () (positions:run (lit "b") 'ab 0)))
; The library's Runs don't check their input, so the Run a program gets
; does.
(check-equal? ((positions:parser-run (lit "b")) "ab" 1)
              (matched "b" 2))
(check-exn exn:fail:contract? (λ () ((positions:parser-run (lit "b")) 'ab 0)))
; A parser made with `parser` is given the input's text itself, not a
; copy.
(define seen #f)
(define spy
  (parser (lambda (in i)
            (set! seen (cons (input-text in) i))
            (matched '() i))))
(define spied "ab!")
(check-equal? (begin ((seq (lit "ab") spy) spied)
                     (and (eq? (car seen) spied) (cdr seen)))
              2)
(check-equal? (object-name (positions:parser (lambda (in i) 'error) 'NAMED))
              'NAMED)
(check-equal? (object-name (positions:parser (lambda (in i) 'error))) #f)
; A plain-function parser can't give back more text than it was given.
(define (GROW s)
  (result 'g (string-append "zz" s)))
(check-exn (message-is "run: parser's remaining is longer than its input\n"
                       "  parser: #<procedure:GROW>\n"
                       "  input: \"bc\"\n"
                       "  remaining: \"zzbc\"")
           (λ () ((unwrap (lit "a") GROW (lit "c")) "abc")))
(define (BACK-ONE s)
  (result 'b (string-append "x" s)))
(check-exn exn:fail:contract?
           (λ () ((unwrap (lit "a") BACK-ONE (lit "a")) "ab")))

; parse tests
(check-equal? (parse (emp) "") '())
(check-exn (message-is "bad input, got error")
           (λ () (parse (lit "a") "b")))
(check-exn (message-is "bad input, had leftover text \"{}\"")
           (λ () (parse (emp) "{}")))

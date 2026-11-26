#lang racket
(require rackunit
         racket/runtime-path
         "../parse.rkt"
         "../lex.rkt"
         (prefix-in chars: (only-in "../json.rkt" string->json))
         "../json-lex.rkt"
         (only-in "../json.rkt" NUMBER STRING)
         (submod "../parse.rkt" private))

; Lexer tests
(check-equal? (tokens json-lexer "") '())
(check-equal? (tokens json-lexer " \n\t ") '())
(check-equal? (tokens json-lexer "[1, \"a\"]")
              (list (token 'begin-array "[" 0 1)
                      (token 'number 1 1 2)
                      (token 'comma "," 2 3)
                      (token 'string "a" 4 7)
                      (token 'end-array "]" 7 8)))
(check-equal? (tokens json-lexer " {\"k\":null}\n")
              (list (token 'begin-object "{" 1 2)
                      (token 'string "k" 2 5)
                      (token 'colon ":" 5 6)
                      (token 'null 'null 6 10)
                      (token 'end-object "}" 10 11)))
(check-exn #rx"position 7" (λ () (tokens json-lexer "[1,\n 2 @]")))

; Error tests
; A failed parse says what it found where it stopped.
(check-exn #rx"unexpected text \"@]\" at line 2, column 4"
           (λ () (string->json "[1,\n 2 @]")))
(check-exn #rx"unexpected number at line 1, column 4" (λ () (string->json "[1 2]")))
(check-exn #rx"unexpected end-array at line 1, column 4" (λ () (string->json "[1,]")))
(check-exn #rx"unexpected end of input at line 1, column 3" (λ () (string->json "[1")))
(check-exn #rx"unexpected begin-object at line 1, column 4" (λ () (string->json "{} {}")))
(check-exn #rx"unexpected text \"nul\" at line 1, column 1" (λ () (string->json "nul")))

; line+column tests
(check-equal? (call-with-values (λ () (line+column "ab\ncd" 0)) list) '(1 1))
(check-equal? (call-with-values (λ () (line+column "ab\ncd" 2)) list) '(1 3))
(check-equal? (call-with-values (λ () (line+column "ab\ncd" 3)) list) '(2 1))

; tok tests
(define toks (json-lexer "[1, \"a\"]"))
(check-equal? (parse (seq (tok 'begin-array) (tok 'number) (tok 'comma)
                          (tok 'string) (tok 'end-array))
                     toks)
              '("[" 1 "," "a" "]"))
(check-equal? (parse (seq (tok 'begin-array) (tok 'number 1) (tok 'comma)
                          (tok 'string "a") (tok 'end-array))
                     toks)
              '("[" 1 "," "a" "]"))
(check-exn exn:fail? (λ () (parse (tok 'number 2) (json-lexer "1"))))
(check-exn exn:fail? (λ () (parse (tok 'number) (json-lexer "\"a\""))))
(check-exn exn:fail? (λ () (parse (tok 'number) (json-lexer ""))))
(check-exn #rx"unexpected comma" (λ () (parse (tok 'number) (json-lexer "1,"))))
; A Parser applied to Tokens leaves the rest of them.
(check-equal? (let ([r ((tok 'number) (json-lexer "1 2"))])
                (list (result-value r) (tokens json-lexer (lexed-string (result-remaining r)))))
              (list 1 (list (token 'number 2 1 2))))

; Start tests
; alt picks its alternatives by the next Token's kind, as it picks them by
; the next character in a String.
(define (start-kinds p)
  (let ([st (start-of p)])
    (and st (list (sort (remove-duplicates (start-chars st)) symbol<?)
                  (start-empty? st)))))
(check-equal? (start-kinds VALUE)
              (list '(begin-array begin-object false null number string true) #f))
(check-equal? (start-kinds ARRAY) (list '(begin-array) #f))
(check-equal? (start-kinds OBJECT) (list '(begin-object) #f))
(check-equal? (start-kinds PAIR) (list '(string) #f))

; Rules are tried in order, and the first to match wins, not the longest,
; so a keyword's rule splits an identifier that begins with it.
(define words
  (lexer #:skip (skip " ")
         [if (lit "if")]
         [ident (span "abcdefghijklmnopqrstuvwxyz")]))
(check-equal? (map token-kind (tokens words "if iffy x"))
              '(if if ident ident))
; Lexing keywords as identifiers, and matching them with `tok`'s value,
; avoids that.
(define idents
  (lexer #:skip (skip " ")
         [ident (span "abcdefghijklmnopqrstuvwxyz")]))
(check-equal? (parse (seq (tok 'ident "if") (tok 'ident) (tok 'ident))
                     (idents "if iffy x"))
              '("if" "iffy" "x"))
(check-exn exn:fail? (λ () (parse (seq (tok 'ident "if") (tok 'ident))
                                  (idents "iffy x"))))

; The grammar never sees whitespace, so a lexer that also drops comments
; parses JSON with comments with the same grammar.
(define jsonc-lexer
  (lexer #:skip (star (alt (span " \t\n\r")
                           (seq (lit "//") (skip (λ (c) (not (char=? c #\newline)))))))
         [begin-object (lit "{")]
         [end-object (lit "}")]
         [begin-array (lit "[")]
         [end-array (lit "]")]
         [comma (lit ",")]
         [colon (lit ":")]
         [null (pmap (const 'null) (lit "null"))]
         [true (pmap (const #t) (lit "true"))]
         [false (pmap (const #f) (lit "false"))]
         [number NUMBER]
         [string STRING]))
(check-equal? (parse VALUE (jsonc-lexer "// a list\n[1, // one\n 2]\n// done"))
              '(1 2))

; Same as json.rkt
; Each text parses to the same value as with json.rkt's string->json, or
; fails with both.
(define (same-as-chars? text)
  (let ([value (λ (f) (with-handlers ([exn:fail? (λ (e) 'failed)]) (f text)))])
    (equal? (value string->json) (value chars:string->json))))

(for ([text (in-list
             (list "null" "true" "false" "0" "-0" "12" "-3.25e+2" "1E-2"
                   "\"\"" "\"a\\n\\u0041\\uD83D\\uDE00\"" "[]" "[ ]" " [ \n ] "
                   "[1,2,3]" "[1, [2, [3]], {}]" "{}" "{ }" "{\"a\":1}"
                   "{\"a\" : 1 , \"b\" : [ ] , \"c\" : { } }"
                   "{\"a\":1,\"b\":2,\"a\":3}" " \t\n1\r\n"
                   ; Errors
                   "" " " "[" "]" "[1,]" "[,1]" "[1 2]" "{\"a\"}" "{\"a\":}"
                   "{1:2}" "{\"a\":1,}" "nul" "tru" "01" "1." "1e" "[1.]"
                   "\"unterminated" "\"\\x\"" "[1]]" "{} {}" "@"))])
  (check-true (same-as-chars? text) text))

(define-runtime-path fixtures "fixtures")
(for ([name (in-list '("canada.json" "citm_catalog.json" "twitter.json"))])
  (check-true (same-as-chars? (file->string (build-path fixtures name))) name))

; Given a String, a lexer skips its characters, ASCII or not.
(define nbsp-lexer
  (lexer #:skip "  "
         [comma (lit ",")]
         [number (span "0123456789")]))
(check-equal? (map token-value (tokens nbsp-lexer " 1 ,  2 "))
              '("1" "," "2"))

; Mixing tests
; Character parsers work in a grammar over Tokens, from the same
; positions. They match exactly what's there, so unlike a Token, they
; don't skip whitespace before themselves.
(define names
  (lexer #:skip " "
         [ident (span "abcdefghijklmnopqrstuvwxyz")]))
(define address (seq (tok 'ident) (lit "@") (tok 'ident)))
(check-equal? (parse address (names "a@b")) '("a" "@" "b"))
(check-equal? (parse address (names " a@ b ")) '("a" "@" "b"))
(check-exn exn:fail? (λ () (parse address (names "a @b"))))
; alt tries a character parser whatever the next Token is, even where
; no Token begins.
(check-equal? (parse (star (alt (tok 'ident) (lit "#"))) (names "a#b#"))
              '("a" "#" "b" "#"))
; json.rkt's STRING is a character parser, so it works over Tokens too.
(check-equal? (parse (seq (tok 'ident) (lit ":") STRING) (names "k:\"\\u0041\""))
              '("k" ":" "A"))
; with-lexer goes the other way: a grammar over characters uses one over
; Tokens for part of its text.
(define setting
  (seq (span "abcdefghijklmnopqrstuvwxyz") (lit "=") (with-lexer json-lexer VALUE)))
(check-equal? (parse setting "a=[1, {\"b\" : null} ]") '("a" "=" (1 #hasheq((b . null)))))
(check-exn exn:fail? (λ () (parse setting "a =[1]")))
; Or a grammar over Tokens uses another lexer's.
(check-equal? (parse (seq (tok 'ident) (with-lexer json-lexer VALUE)) (names "x [1, 2]"))
              '("x" (1 2)))

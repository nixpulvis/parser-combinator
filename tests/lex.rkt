#lang racket
(require rackunit
         "../parse.rkt"
         "../lex.rkt"
         "../json.rkt"
         (submod "../json.rkt" tokens)
         (submod "../parse.rkt" private))

; These use json.rkt's lexer and its grammar over Tokens, and its NUMBER
; and STRING as parsers over characters to mix with them.

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
                (list (result-value r) (tokens json-lexer (input-text (result-remaining r)))))
              (list 1 (list (token 'number 2 1 2))))

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
; Any symbol can be a kind, even one that reads like the end of the text.
(define ends
  (lexer #:skip " "
         [eof (lit "end")]
         [ident (span "abcdefghijklmnopqrstuvwxyz")]))
(check-equal? (map token-kind (tokens ends "ab end")) '(ident eof))
(check-equal? (parse (seq (tok 'ident) (tok 'eof)) (ends "ab end")) '("ab" "end"))
(check-exn #rx"unexpected end of input at line 1, column 3"
           (λ () (parse (seq (tok 'ident) (tok 'eof)) (ends "ab"))))
; Without #:skip, a lexer skips nothing.
(define bare (lexer [a (lit "a")]))
(check-equal? (map token-kind (tokens bare "aa")) '(a a))
(check-exn #rx"tokens: unexpected text \" a\" at line 1, column 2"
           (λ () (tokens bare "a a")))

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
(check-equal? (parse JSON-VALUE (jsonc-lexer "// a list\n[1, // one\n 2]\n// done"))
              '(1 2))

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
; A character parser's failure counts toward where the parse got
; furthest, so it's reported there, at what comes next.
(check-exn #rx"unexpected text \"@b\" at line 1, column 3"
           (λ () (parse address (names "a @b"))))
; alt tries a character parser whatever the next Token is, even where
; no Token begins.
(check-equal? (parse (star (alt (tok 'ident) (lit "#"))) (names "a#b#"))
              '("a" "#" "b" "#"))
; json.rkt's STRING is a character parser, so it works over Tokens too.
(check-equal? (parse (seq (tok 'ident) (lit ":") STRING) (names "k:\"\\u0041\""))
              '("k" ":" "A"))
; And a parser with a Run written by hand, which reads the text with
; input-text and passes its Input on to run, so Tokens work inside it.
(define (twice p)
  (parser (lambda (in i)
            (let ([m (run p in i)])
              (if (symbol? m)
                  'error
                  (let ([m2 (run p in (matched-end m))])
                    (if (symbol? m2)
                        'error
                        (matched (list (matched-value m) (matched-value m2))
                                 (matched-end m2)))))))))
(check-equal? (parse (twice (tok 'ident)) (names " a  b ")) '("a" "b"))
(define AT-RUN
  (parser (lambda (in i)
            (let ([s (input-text in)])
              (if (and (< i (string-length s)) (char=? (string-ref s i) #\@))
                  (matched "@" (add1 i))
                  'error)))))
(check-equal? (parse (seq (tok 'ident) AT-RUN (tok 'ident)) (names "a@b"))
              '("a" "@" "b"))
; A grammar over Tokens raises an error given a string.
(check-exn #rx"^tok: input was not made by a lexer$" (λ () (parse JSON-VALUE "null")))
(check-exn #rx"^tok: input was not made by a lexer$" (λ () ((tok 'ident) "a")))
; A lexer is one `lexer` made, and anything else is caught when it's used.
(check-exn exn:fail:contract? (λ () (with-lexer (λ (s) 'oops) (lit "a"))))
(check-exn exn:fail:contract? (λ () (tokens (λ (s) s) "a")))
(check-true (lexer? json-lexer))
; with-lexer goes the other way: a grammar over characters uses one over
; Tokens for part of its text.
(define setting
  (seq (span "abcdefghijklmnopqrstuvwxyz") (lit "=") (with-lexer json-lexer JSON-VALUE)))
(check-equal? (parse setting "a=[1, {\"b\" : null} ]") '("a" "=" (1 #hasheq((b . null)))))
(check-exn exn:fail? (λ () (parse setting "a =[1]")))
; What its lexer skips belongs to the Token after it, so the whitespace
; after the match is left to the grammar around it.
(check-equal? (parse (seq setting (lit " ;")) "a=[1] ;") '(("a" "=" (1)) " ;"))
(check-exn #rx"unexpected text \" ;\" at line 1, column 6"
           (λ () (parse (seq setting (lit ";")) "a=[1] ;")))
; Where the parse got furthest in the grammar over Tokens, the error says
; it in Tokens.
(check-exn #rx"unexpected number at line 1, column 6"
           (λ () (parse setting "a=[1 2]")))
; Or a grammar over Tokens uses another lexer's.
(check-equal? (parse (seq (tok 'ident) (with-lexer json-lexer JSON-VALUE)) (names "x [1, 2]"))
              '("x" (1 2)))
; JSON's VALUE brings json-lexer with it, so it works in any grammar.
(check-equal? (parse (seq (lit "a=") VALUE (lit ";")) "a= [1];") '("a=" (1) ";"))
(check-equal? (parse (seq (tok 'ident) VALUE) (names "x [1]")) '("x" (1)))

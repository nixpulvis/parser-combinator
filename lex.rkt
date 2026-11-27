#lang racket/base
(require (only-in "parse.rkt" [skip skip-chars])
         (only-in (submod "parse.rkt" private)
                  [parser parser-with-start] token struct:token token? token-kind token-value
                  token-start token-end source-text source-match source-furthest make-source
                  kept-value kept-start kept-end peek! peek-id! ->source reach! run parser?
                  parser-run matched matched-end start start-of start-chars start-empty?
                  kind-id line+column))

(provide lexer
         tok
         tokens
         with-lexer
         (struct-out token))

; ---------------------------
; ----- Lexing to Tokens -----
; ---------------------------

; A lexer is the first of two stages. It splits a String into Tokens,
; dropping what the grammar never needs to see, like whitespace and
; comments, and a grammar written with `tok` then parses the Tokens.
; The rules of a lexer are themselves parsers over characters, so a
; lexer is written with the same combinators as any other grammar.
;
; The stages are interleaved: a Token is scanned when the grammar asks
; for it, from the String, so no list of Tokens is made.

; A Lexer is a [String -> Source]
; that makes a Source of the String with its Scanner, for `parse` and
; the grammar's `tok`s. To a program, the Source is an Input.

; A Token is a (token Symbol Any Natural Natural), from parse.rkt.

; (lexer #:skip skip [kind parser] ...) : Lexer
; Makes a Lexer whose Tokens come after whatever `skip` matches, or,
; given a String, after any run of its characters, which is faster.
; Each Token is matched by the first rule's `parser` that matches, and
; has that rule's `kind`, and the parser's Value as its value. As in
; `alt`, the rules are tried in order and the first to match wins, so a
; keyword's rule goes before the identifiers' rule, and only the rules
; that can begin with the next character are tried.
(define-syntax-rule (lexer #:skip skip [kind p] ...)
  (make-lexer skip (list (cons 'kind p) ...)))

; (or/c String Parser) [Listof (cons Symbol Parser)] -> Lexer
(define (make-lexer skip rules)
  (let ([scanner (make-scanner skip rules)])
    (lambda (s)
      (make-source s scanner))))

; (or/c String Parser) [Listof (cons Symbol Parser)] -> Scanner
; A Scanner is a [Source Natural -> (values (or/c Natural #f) Natural Match)]
; that skips what `skip` matches from position `i` of the Source's text,
; and then matches the first of `rules` that matches. It produces that
; rule's Kind ID, where its Token begins, and its Match, or the ID of
; 'eof at the end of the text, or #f and 'error where nothing matches.
; Its Source is one with no lexer, since its rules are parsers over
; characters.
;
; Like `alt`, it tries only the rules whose Start says they can begin
; with the next character, and `skip` is one of them, tried first, so
; where there's nothing to skip it costs no more than that check. When
; `skip` matches something, the rules are picked again after it. It
; knows which rule matched, so it needn't wrap each rule's Value with its
; kind.
(define (make-scanner skip rules)
  (let* ([eof (kind-id 'eof)]
         ; Given a String, the ASCII characters in it are skipped by a
         ; loop here, as `skip` does, and any others by a `skip` rule.
         [space (if (string? skip)
                    (for/vector #:length 128 ([n (in-range 128)])
                      (and (memv (integer->char n) (string->list skip)) #t))
                    (make-vector 128 #f))]
         [skip (if (string? skip)
                   (let ([others (filter (lambda (c) (>= (char->integer c) 128))
                                         (string->list skip))])
                     (and (pair? others) (skip-chars (list->string others))))
                   skip)]
         ; A #f kind marks `skip`.
         [rules (append (if skip (list (cons #f skip)) '())
                        (for/list ([r (in-list rules)])
                          (cons (kind-id (car r)) (cdr r))))]
         [sts (for/list ([r (in-list rules)]) (start-of (cdr r)))]
         ; [Maybe Char] -> [Listof (cons (or/c Natural #f) Parser)]
         ; `skip` can match nothing, but only matters where it matches
         ; something, so only its characters count.
         [for-char
          (lambda (c)
            (for/list ([r (in-list rules)] [st (in-list sts)]
                       #:when (or (not st)
                                  (and (car r) (start-empty? st))
                                  (and c (memv c (start-chars st)))))
              (cons (car r) (as-run (cdr r)))))]
         [otherwise (for-char #f)]
         [ascii (for/vector #:length 128 ([n (in-range 128)])
                  (for-char (integer->char n)))]
         [others (for*/hasheqv ([st (in-list sts)] #:when st
                                [c (in-list (start-chars st))]
                                #:when (>= (char->integer c) 128))
                   (values c (for-char c)))])
    (lambda (in i)
      (let ([s (source-text in)])
        (let next ([i i])
          (if (= i (string-length s))
              (values eof i 'error)
              (let* ([c (string-ref s i)]
                     [n (char->integer c)])
                (if (and (< n 128) (vector-ref space n))
                    (next (add1 i))
                    (let loop ([rs (if (< n 128)
                                       (vector-ref ascii n)
                                       (hash-ref others c (lambda () otherwise)))])
                      (if (null? rs)
                          (values #f i 'error)
                          (let ([m ((cdar rs) in i)])
                            (cond [(or (symbol? m) (= (matched-end m) i))
                                   (loop (cdr rs))]
                                  [(caar rs) (values (caar rs) i m)]
                                  [else (next (matched-end m))]))))))))))))

; Symbol -> Parser
; Symbol Any -> Parser
; Matches one Token of the kind `kind`, producing its value. Given a
; `value` too, it only matches a Token whose value is `equal?` to it,
; such as a keyword among the identifiers. It needs a lexer's Source,
; and given one without, it raises an error.
(define tok
  (case-lambda
    [(kind)
     (let ([id (kind-id kind)])
       (parser-with-start
        (lambda (in i)
          (if (eq? (peek-id! in i) id)
              (source-match in)
              'error))
        (start (list kind) #f)
        kind))]
    [(kind value)
     (let ([id (kind-id kind)])
       (parser-with-start
        (lambda (in i)
          (if (and (eq? (peek-id! in i) id)
                   (equal? (kept-value in) value))
              (matched value (kept-end in))
              'error))
        (start (list kind) #f)
        kind))]))

; Lexer Parser -> Parser
; Matches what `p` does, over the Tokens `lex` finds from where it's
; run. It lets a grammar over characters use one over Tokens for part of
; its text, such as a JSON value after a key in a config file, and a
; grammar over Tokens use another lexer's for part of its own, such as
; the code inside a string. Positions are the same in either, so `p`
; picks up where the grammar around it is, and that grammar picks up
; where `p` ends. How far `p` got counts toward how far the parse got.
; Nothing is known about what `p` begins with in characters, so `alt`
; always tries it.
(define (with-lexer lex p)
  (parser-with-start
   (lambda (in i)
     (let* ([inner (->source 'with-lexer (lex (source-text in)))]
            [m (run p inner i)])
       (reach! in (source-furthest inner))
       m))
   #f))

; Parser -> Run
(define (as-run p)
  (if (parser? p) (parser-run p) (lambda (in i) (run p in i))))

; Lexer String -> [Listof Token]
; The Tokens `lex` finds in `s`, to see what a grammar over them sees. It
; raises an error where no rule matches.
(define (tokens lex s)
  (let ([in (->source 'tokens (lex s))])
    (let loop ([i 0] [acc '()])
      (let ([kind (peek! in i)])
        (case kind
          [(eof) (reverse acc)]
          [(#f) (let-values ([(line column) (line+column s (kept-start in))])
                  (error 'tokens "no token matches at line ~a, column ~a" line column))]
          [else (loop (kept-end in)
                      (cons (token kind (kept-value in) (kept-start in) (kept-end in))
                            acc))])))))

#lang racket/base
(require (only-in "parse.rkt" token [skip skip-chars])
         (only-in (submod "parse.rkt" private)
                  make-lexed lexed-value lexed-start lexed-end peek!
                  run parser? parser-run matched-end start-of start-chars start-empty? kind-id))

(provide lexer
         tokens)

; ---------------------------
; ----- Lexing to Tokens -----
; ---------------------------

; A lexer is the first of two stages. It splits a String into Tokens,
; dropping what the grammar never needs to see, like whitespace and
; comments, and a grammar written with `tok` then parses the Tokens.
; The rules of a lexer are themselves parsers over characters, so a
; lexer is written with the same combinators as any other grammar.
;
; The stages are interleaved: a Token is lexed when the grammar asks
; for it, from the String, so no list of Tokens is made.

; A Lexer is a [String -> Lexed]
; which `parse` and `tok` take as their input.

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
  (let ([rule (rule-chooser skip rules)])
    (lambda (s)
      (make-lexed s rule))))

; (or/c String Parser) [Listof (cons Symbol Parser)] -> Rule
; A Rule is a [String Natural -> (values (or/c Natural #f) Natural Match)]
; that skips what `skip` matches from position `i` of `s`, and then
; matches the first of `rules` that matches. It produces that rule's Kind
; ID, where its Token begins, and its Match, or the ID of 'eof at the end
; of `s`, or #f and 'error where nothing matches.
;
; Like `alt`, it tries only the rules whose Start says they can begin
; with the next character, and `skip` is one of them, tried first, so
; where there's nothing to skip it costs no more than that check. When
; `skip` matches something, the rules are picked again after it. It
; knows which rule matched, so it needn't wrap each rule's Value with its
; kind.
(define (rule-chooser skip rules)
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
    (lambda (s i)
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
                    (let ([m ((cdar rs) s i)])
                      (cond [(or (symbol? m) (= (matched-end m) i))
                             (loop (cdr rs))]
                            [(caar rs) (values (caar rs) i m)]
                            [else (next (matched-end m))])))))))))))

; Parser -> Run
(define (as-run p)
  (if (parser? p) (parser-run p) (lambda (s i) (run p s i))))

; Lexer String -> [Listof Token]
; The Tokens `lex` finds in `s`, to see what a grammar over them sees. It
; raises an error where no rule matches.
(define (tokens lex s)
  (let ([in (lex s)])
    (let loop ([i 0] [acc '()])
      (let ([kind (peek! in i)])
        (case kind
          [(eof) (reverse acc)]
          [(#f) (error 'tokens "no token matches at position ~a" (lexed-start in))]
          [else (loop (lexed-end in)
                      (cons (token kind (lexed-value in) (lexed-start in) (lexed-end in))
                            acc))])))))

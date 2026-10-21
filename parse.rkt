#lang racket

(provide define-parser
         lit
         rx
         alt
         alt*
         seq
         seq*
         opt
         emp
         plus
         star
         unwrap
         parse
         flatten-ast
         ; TODO: Remove this provide.
         (struct-out result))

; ----------------------------------------
; ----- Context Free Grammar Library -----
; ----------------------------------------

; An AST is whatever a parser produces:
; - `lit` and `rx` produce the matched String
; - `emp`, and `opt` when its parser fails, produce '()
; - `star` and `plus` produce the list of their parser's ASTs
; - `seq` and `seq*` produce whatever their constructor returns

; A Result is one of:
; - 'error
; - (result AST String)
(struct result (node remaining) #:transparent)

; A Parser is a [String -> Result]

; Parser -> Parser : Transformation
; Makes things lazy.
(define-syntax-rule (define-parser name body)
  (define (name s)
    (body s)))

; String -> Parser
(define (lit str)
  (lambda (s)
    (let ([len (string-length str)])
      (if (and (>= (string-length s) len)
               (string=? (substring s 0 len) str))
          (result str (substring s len))
          'error))))

; Regexp -> Parser
; Matches `pattern` at the start of the input.
(define (rx pattern)
  (unless (regexp? pattern)
    (raise-argument-error 'rx "regexp?" pattern))
  (let ([anchored ((if (pregexp? pattern) pregexp regexp)
                   (string-append "^(?:" (object-name pattern) ")"))])
    (lambda (s)
      (let ([m (regexp-match anchored s)])
        (if m
            (result (car m) (substring s (string-length (car m))))
            'error)))))

; Parser Parser -> Parser
(define (alt p1 p2)
  (lambda (s)
    (let ([r1 (p1 s)])
      (if (symbol? r1) (p2 s) r1))))

; Parser Parser ... -> Parser
(define (alt* parser . parsers)
  (lambda (s)
    ((if (empty? parsers)
            parser
            (alt parser (apply alt* parsers))) s)))

; Symbol Any Natural -> Void
; Raises an error for `who` unless `build` is a constructor that accepts
; `n` ASTs.
(define (check-constructor who build n)
  (unless (and (procedure? build) (procedure-arity-includes? build n))
    (raise-argument-error who
                          (format "(procedure-arity-includes/c ~a)" n)
                          build)))

; (AST AST -> AST) Parser Parser -> Parser
(define (seq build p1 p2)
  (check-constructor 'seq build 2)
  (lambda (s)
    (let ([r1 (p1 s)])
      (if (symbol? r1)
          'error
          (let ([r2 (p2 (result-remaining r1))])
            (if (symbol? r2)
                'error
                (result (build (result-node r1) (result-node r2))
                        (result-remaining r2))))))))

; (AST ... -> AST) Parser Parser ... -> Parser
; The constructor gets one AST per parser, collected into a list by
; nesting `seq` with `cons`.
(define (seq* build parser . parsers)
  (check-constructor 'seq* build (add1 (length parsers)))
  (seq (lambda (ast asts) (apply build ast asts))
       parser
       (foldr (lambda (p rest) (seq cons p rest))
              (lambda (s) (result '() s))
              parsers)))

; Parser -> Parser
(define (opt parser)
  (lambda (s)
    (let ([r (parser s)])
      (if (symbol? r) (result '() s) r))))

; -> Parser
(define (emp)
  (lambda (s)
    (result '() s)))

; Parser -> Parser
(define (plus parser)
  (lambda (s)
    ((seq cons parser (star parser)) s)))

; Parser -> Parser
(define (star parser)
  (lambda (s)
    ((alt (seq cons parser (lambda (s) (if (string=? s "")
                                          (result '() "")
                                          ((star parser) s))))
         (emp)) s)))

; Parser Parser Parser -> Parser
(define (unwrap left body right)
  (seq* (lambda (l b r) b) left body right))

; Parser String -> AST
(define (parse parser str)
  (let ([r (parser str)])
    (cond [(symbol? r)
           (error "bad input, got error")]
          [(> (string-length (result-remaining r)) 0)
           (error "bad input, had leftover text" (result-remaining r))]
          [else
           (result-node r)])))

; AST -> String
; Appends the strings in `ast`, which is a string or a list of such ASTs.
(define (flatten-ast ast)
  (cond [(string? ast) ast]
        [(list? ast) (apply string-append (map flatten-ast ast))]
        [else (error "bad input: given" ast)]))

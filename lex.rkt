#lang racket

(provide (struct-out emp-node)
         (struct-out lit-node)
         (struct-out wrap-node)
         (struct-out seq-node)
         lit
         char-except
         alt
         alt*
         seq
         seq*
         opt
         emp
         plus
         star
         wrap
         unwrap
         lex
         star-foldr
         star-foldl
         flatten-ast
         ; TODO: Remove this provide.
         (struct-out result))

; ----------------------------------------
; ----- Context Free Grammar Library -----
; ----------------------------------------

; A AST is one of:
; - #<emp-node>
; - #<lit-node String>
; - #<wrap-node Symbol AST>
; - #<seq-node Symbol AST AST>
(struct emp-node () #:transparent)
(struct lit-node (value) #:transparent)
(struct wrap-node (label value) #:transparent)
(struct seq-node (label left right) #:transparent)

; A Result is one of:
; - 'error
; - (result AST String)
(struct result (node remaining) #:transparent)

; A Lexer is a [String -> Result]

; String -> Lexer
(define (lit str)
  (lambda (s)
    (let ([len (string-length str)])
      (if (and (>= (string-length s) len)
               (string=? (substring s 0 len) str))
          (result (lit-node str) (substring s len))
          'error))))

; String ... -> Lexer
(define (char-except . chars)
  (lambda (s)
    (if (and (>= (string-length s) 1)
             (not (member (substring s 0 1) chars)))
        (result (lit-node (substring s 0 1)) (substring s 1 (string-length s)))
        'error)))

; Lexer Lexer -> Lexer
(define (alt p1 p2)
  (lambda (s)
    (let ([r1 (p1 s)])
      (if (symbol? r1) (p2 s) r1))))

; Lexer Lexer ... -> Lexer
(define (alt* lexer . lexers)
  (lambda (s)
    ((if (empty? lexers)
            lexer
            (alt lexer (apply alt* lexers))) s)))

; Symbol Lexer Lexer -> Lexer
(define (seq label p1 p2)
  (lambda (s)
    (let ([r1 (p1 s)])
      (if (symbol? r1)
          'error
          (let ([r2 (p2 (result-remaining r1))])
            (if (symbol? r2)
                'error
                (result (seq-node label (result-node r1) (result-node r2))
                        (result-remaining r2))))))))

; Symbol Lexer Lexer ... -> Lexer
(define (seq* label lexer . lexers)
  (lambda (s)
    ((if (empty? lexers)
         lexer
         (seq label lexer (apply seq* label lexers))) s)))

; Lexer -> Lexer
(define (opt lexer)
  (lambda (s)
    (let ([r (lexer s)])
      (if (symbol? r) (result (emp-node) s) r))))

; -> Lexer
(define (emp)
  (lambda (s)
    (result (emp-node) s)))

; Lexer -> Lexer
(define (plus lexer)
  (lambda (s)
    ((seq 'rep lexer (star lexer)) s)))

; Lexer -> Lexer
(define (star lexer)
  (lambda (s)
    ((alt (seq 'rep lexer (lambda (s) (if (string=? s "")
                                          (result (emp-node) "")
                                          ((star lexer) s))))
         (emp)) s)))

; Symbol Lexer Lexer Lexer -> Lexer
(define (wrap label l b r)
  (lambda (s)
    (let ([r ((seq* 'wrap l b r) s)])
      (if (symbol? r)
           'error
           (result (wrap-node label
                             (seq-node-left (seq-node-right (result-node r))))
                   (result-remaining r))))))

; Lexer Lexer Lexer -> Lexer
(define (unwrap l b r)
  (lambda (s)
    (let ([r ((seq* 'wrap l b r) s)])
      (if (symbol? r)
           'error
           (result (seq-node-left (seq-node-right (result-node r)))
                   (result-remaining r))))))

; Lexer String -> AST
(define (lex lexer str)
  (let ([r (lexer str)])
    (cond [(symbol? r)
           (error "bad input, got error")]
          [(> (string-length (result-remaining r)) 0)
           (error "bad input, had leftover text" (result-remaining r))]
          [else
           (result-node r)])))

; (AST X -> X) X (seq Symbol AST AST) -> X
(define (star-foldr op base ast)
  (if (and (seq-node? ast)
           (symbol=? (seq-node-label ast) 'rep))
      (op (seq-node-left ast) (star-foldr op base (seq-node-right ast)))
      base))

; (AST X -> X) X (seq Symbol AST AST) -> X
(define (star-foldl op base ast)
  (if (and (seq-node? ast)
           (symbol=? (seq-node-label ast) 'rep))
      (star-foldl op (op (seq-node-left ast) base) (seq-node-right ast))
      base))

; AST -> String
(define (flatten-ast ast)
  (cond [(emp-node? ast) ""]
        [(lit-node? ast) (lit-node-value ast)]
        [(wrap-node? ast) (flatten-ast (wrap-node-value ast))]
        [(seq-node? ast) (string-append (flatten-ast (seq-node-left ast))
                                        (flatten-ast (seq-node-right ast)))]
        [else (error "bad input: given" ast)]))

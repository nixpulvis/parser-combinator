#lang racket

(provide (struct-out emp-node)
         (struct-out lit-node)
         (struct-out seq-node)
         define-parser
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
; - #<seq-node Symbol AST AST>
; - any value that a `seq` or `seq*` constructor produces
(struct emp-node () #:transparent)
(struct lit-node (value) #:transparent)
(struct seq-node (label left right) #:transparent)

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
          (result (lit-node str) (substring s len))
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
            (result (lit-node (car m)) (substring s (string-length (car m))))
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
; Raises an error for `who` unless `build` is a label or a constructor
; that accepts `n` ASTs.
(define (check-label-or-constructor who build n)
  (unless (or (symbol? build)
              (and (procedure? build) (procedure-arity-includes? build n)))
    (raise-argument-error who
                          (format "(or/c symbol? (procedure-arity-includes/c ~a))" n)
                          build)))

; [Symbol or (AST AST -> AST)] Parser Parser -> Parser
(define (seq build p1 p2)
  (check-label-or-constructor 'seq build 2)
  (let ([build (if (symbol? build)
                   (lambda (left right) (seq-node build left right))
                   build)])
    (lambda (s)
      (let ([r1 (p1 s)])
        (if (symbol? r1)
            'error
            (let ([r2 (p2 (result-remaining r1))])
              (if (symbol? r2)
                  'error
                  (result (build (result-node r1) (result-node r2))
                          (result-remaining r2)))))))))

; [Symbol or (AST ... -> AST)] Parser Parser ... -> Parser
; A label nests `seq` to the right. A constructor gets one AST per parser,
; collected into a list by nesting `seq` with `cons`.
(define (seq* build parser . parsers)
  (check-label-or-constructor 'seq* build (add1 (length parsers)))
  (if (symbol? build)
      (lambda (s)
        ((if (empty? parsers)
             parser
             (seq build parser (apply seq* build parsers))) s))
      (seq (lambda (ast asts) (apply build ast asts))
           parser
           (foldr (lambda (p rest) (seq cons p rest))
                  (lambda (s) (result '() s))
                  parsers))))

; Parser -> Parser
(define (opt parser)
  (lambda (s)
    (let ([r (parser s)])
      (if (symbol? r) (result (emp-node) s) r))))

; -> Parser
(define (emp)
  (lambda (s)
    (result (emp-node) s)))

; Parser -> Parser
(define (plus parser)
  (lambda (s)
    ((seq 'rep parser (star parser)) s)))

; Parser -> Parser
(define (star parser)
  (lambda (s)
    ((alt (seq 'rep parser (lambda (s) (if (string=? s "")
                                          (result (emp-node) "")
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
        [(seq-node? ast) (string-append (flatten-ast (seq-node-left ast))
                                        (flatten-ast (seq-node-right ast)))]
        [else (error "bad input: given" ast)]))

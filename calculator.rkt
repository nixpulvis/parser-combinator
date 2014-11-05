#lang racket
(require "lex.rkt")

; DIGIT ::= "0" | "1" | "2" | "3" | "4" | "5"
;         | "6" | "7" | "8" | "9"
;
; NUMBER ::= DIGIT | DIGIT NUMBER
;
; EXPR ::= NUMBER "+" EXPR
;        | NUMBER "-" EXPR
;        | NUMBER "*" EXPR
;        | NUMBER "/" EXPR
;        | NUMBER

(define (EXPR s)
  ((alt* (seq* 'plus NUMBER (lit "+") EXPR)
         (seq* 'minus NUMBER (lit "-") EXPR)
         (seq* 'times NUMBER (lit "*") EXPR)
         (seq* 'divide NUMBER (lit "/") EXPR)
         NUMBER) s))

(define (NUMBER s)
  ((seq* 'number DIGIT (star DIGIT)) s))

(define (DIGIT s)
  ((alt* (lit "0") (lit "1") (lit "2") (lit "3") (lit "4") (lit "5") (lit "6")
         (lit "7") (lit "8") (lit "9")) s))

; Parsing

; TODO: Need to have better ASTs...

(define (parse-number ast)
  (string->number (flatten-ast ast)))

(define (parse-expr ast)
  (cond [(symbol=? 'number (seq-node-label ast))
         (parse-number ast)]
        [(symbol=? 'plus (seq-node-label ast))
         (+ (parse-number (seq-node-left ast))
            (parse-expr (seq-node-right ast)))]
        [(symbol=? 'minus (seq-node-label ast))
         2]
        [(symbol=? 'times (seq-node-label ast))
         3]
        [(symbol=? 'divide (seq-node-label ast))
         4]))

; (parse-expr (lex EXPR "5+10"))
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

(define EXPR (lambda (s)
  ((alt* (seq* 'plus NUMBER (lit "+") EXPR)
         (seq* 'minus NUMBER (lit "-") EXPR)
         (seq* 'times NUMBER (lit "*") EXPR)
         (seq* 'divide NUMBER (lit "/") EXPR)
         NUMBER) s)))

(define NUMBER (lambda (s)
  ((alt* DIGIT
         (seq* 'digits DIGIT NUMBER)) s)))

(define DIGIT (lambda (s)
  ((alt* (lit "0") (lit "1") (lit "2") (lit "3") (lit "4") (lit "5") (lit "6")
         (lit "7") (lit "8") (lit "9")) s)))

(lex EXPR "1+2/3")

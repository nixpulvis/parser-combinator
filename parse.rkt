#lang typed/racket

; ----------------------------------------
; ----- Context Free Grammar Library -----
; ----------------------------------------

; AST Type
(define-type (AST A) (U (Node A) A))
(struct (A) Node ([left : (AST A)]
                  [right : (AST A)])
                 #:transparent)

; Result Type
(struct (A) Result ([ast : (AST A)]
                    [remaining : String])
                   #:transparent)

; Parser Type
(define-type (Parser A) (String -> (Listof (Result A))))

(: ret : (All (A) String -> (Parser String)))
(define (ret item)
  (lambda (s) (list (Result item ""))))

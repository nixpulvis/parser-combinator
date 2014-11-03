#lang racket
(require test-engine/racket-tests)
(require "lex.rkt")

; lit tests
(check-expect ((lit "-") "-1.35")
              (result (lit-node "-") "1.35"))
(check-expect ((lit "-") "-")
              (result (lit-node "-") ""))
(check-expect ((lit "hi") "high")
              (result (lit-node "hi") "gh"))
(check-expect ((lit "") "")
              (result (lit-node "") ""))
(check-expect ((lit "") "walrus")
              (result (lit-node "") "walrus"))
(check-expect ((lit "-") "hi")
              'error)
(check-expect ((lit "high") "hi")
              'error)
(check-expect ((lit "high") "hippo")
              'error)

; char-except tests
(check-expect ((char-except "a") "b")
              (result (lit-node "b") ""))
(check-expect ((char-except "a") "a")
              'error)
(check-expect ((char-except "a" "b") "a")
              'error)
(check-expect ((char-except "a" "b") "b")
              'error)
(check-expect ((char-except "a" "b") "b")
              'error)
(check-expect ((star (char-except "a" "b" "c")) "def")
              (result (seq-node 'rep (lit-node "d") (seq-node 'rep (lit-node "e") (seq-node 'rep (lit-node "f") (emp-node)))) ""))
(check-expect ((star (char-except "a" "b" "c")) "adc")
              (result (emp-node) "adc"))

; alt tests
(check-expect ((alt (lit "+") (lit "-")) "+123")
              (result (lit-node "+") "123"))
(check-expect ((alt (lit "+") (lit "-")) "-123")
              (result (lit-node "-") "123"))
(check-expect ((alt (lit "+") (alt (lit "-") (lit "."))) ".foo")
              (result (lit-node ".") "foo"))
(check-expect ((alt (lit "hi") (lit "high")) "higher")
              (result (lit-node "hi") "gher"))
(check-expect ((alt (lit "+") (lit "-")) "007")
              'error)

; alt* tests
(check-expect ((alt* (lit "+") (lit "-") (lit ".")) "-123")
              (result (lit-node "-") "123"))
(check-expect ((alt* (lit "+") (lit "-") (lit ".")) "-+.")
              (result (lit-node "-") "+."))
(check-expect ((alt* (lit "1") (lit "12") (lit "123")) "123")
              (result (lit-node "1") "23"))
(check-expect ((alt* (lit "+") (lit "-") (lit ".")) "123")
              'error)

; seq tests
(check-expect ((seq 'a (lit "-") (lit "+")) "-+...")
              (result (seq-node 'a (lit-node "-") (lit-node "+")) "..."))
(check-expect ((seq 'a (lit "-") (lit "+")) "+-...")
              'error)

; seq* tests
(check-expect ((seq* 'a (lit "-") (lit "+")) "-+...")
              (result (seq-node 'a (lit-node "-") (lit-node "+")) "..."))
(check-expect ((seq* 'b (lit "-") (lit "+") (lit ".")) "-+...")
              (result (seq-node 'b
                                (lit-node "-")
                                (seq-node 'b
                                          (lit-node "+")
                                          (lit-node "."))) ".."))
(check-expect ((seq* 'c (lit "-") (lit "+")) "+-...")
              'error)

; opt tests
(check-expect ((opt (lit "-")) "-123")
              (result (lit-node "-") "123"))
(check-expect ((opt (lit "-")) "123")
              (result (emp-node) "123"))
(check-expect ((opt (lit "-")) "")
              (result (emp-node) ""))

; emp tests
(check-expect ((emp) "123")
              (result (emp-node) "123"))
(check-expect ((emp) "")
              (result (emp-node) ""))

; plus tests
(check-expect ((plus (lit "+")) "123")
              'error)
(check-expect ((plus (lit "+")) "+123")
              (result (seq-node 'rep (lit-node "+") (emp-node)) "123"))

; star tests
(check-expect ((star (lit "+")) "123")
              (result (emp-node) "123"))
(check-expect ((star (lit "+")) "+123")
              (result (seq-node 'rep (lit-node "+") (emp-node)) "123"))
(check-expect ((star (lit "+")) "++123")
              (result (seq-node 'rep
                                (lit-node "+")
                                (seq-node 'rep
                                          (lit-node "+")
                                          (emp-node))) "123"))
(check-expect ((star (alt (lit "1") (lit "2"))) "123")
              (result (seq-node 'rep
                                (lit-node "1")
                                (seq-node 'rep
                                          (lit-node "2")
                                          (emp-node))) "3"))
(check-expect ((star (alt (lit "1") (lit "2"))) "12")
              (result (seq-node 'rep
                                (lit-node "1")
                                (seq-node 'rep
                                          (lit-node "2")
                                          (emp-node))) ""))

; wrap tests
(check-expect ((wrap '+ (lit "+") (lit "!") (lit "+")) "+!+")
              (result (wrap-node '+ (lit-node "!")) ""))
(check-expect ((wrap '+ (lit "+") (lit "!") (lit "+")) "+!+++")
              (result (wrap-node '+ (lit-node "!")) "++"))
(check-expect ((wrap '+ (lit "+") (star (lit "!")) (lit "+")) "+!!+++")
              (result (wrap-node '+ (seq-node 'rep
                                              (lit-node "!")
                                              (seq-node 'rep
                                                        (lit-node "!")
                                                        (emp-node)))) "++"))
(check-expect ((wrap '+ (lit "+") (lit "!") (lit "+")) "+!-")
              'error)

; unwrap tests
(check-expect ((unwrap (lit "+") (lit "!") (lit "+")) "+!+")
              (result (lit-node "!") ""))
(check-expect ((unwrap (lit "+") (lit "!") (lit "+")) "+!+++")
              (result (lit-node "!") "++"))
(check-expect ((unwrap (lit "+") (star (lit "!")) (lit "+")) "+!!+++")
              (result (seq-node 'rep
                                 (lit-node "!")
                                 (seq-node 'rep
                                           (lit-node "!")
                                           (emp-node))) "++"))
(check-expect ((unwrap (lit "+") (lit "!") (lit "+")) "+!-")
              'error)

; lex tests
(check-expect (lex (emp) "") (emp-node))
(check-error (lex (lit "a") "b") "bad input, got error")
(check-error (lex (emp) "{}") "bad input, had leftover text \"{}\"")

; star-foldr tests
(check-expect (star-foldr (lambda (ast acc) (cons ast acc))
                          empty
                          (lex (lit "hh") "hh"))
              empty)
(check-expect (star-foldr (lambda (ast acc) (cons ast acc))
                          empty
                          (lex (star (lit "h")) "hh"))
              (list (lit-node "h") (lit-node "h")))
(check-expect (star-foldr (lambda (ast acc) (cons ast acc))
                          empty
                          (lex (star (alt (lit "a") (lit "b"))) "ab"))
              (list (lit-node "a") (lit-node "b")))
(check-expect (star-foldr (lambda (ast acc) (cons ast acc))
                          empty
                          (lex (star (seq 'ab (lit "a") (lit "b"))) "abab"))
              (list (seq-node 'ab (lit-node "a") (lit-node "b"))
                    (seq-node 'ab (lit-node "a") (lit-node "b"))))

; star-foldl tests
(check-expect (star-foldl (lambda (ast acc) (cons ast acc))
                          empty
                          (lex (lit "hh") "hh"))
              empty)
(check-expect (star-foldl (lambda (ast acc) (cons ast acc))
                          empty
                          (lex (star (lit "h")) "hh"))
              (list (lit-node "h") (lit-node "h")))
(check-expect (star-foldl (lambda (ast acc) (cons ast acc))
                          empty
                          (lex (star (alt (lit "a") (lit "b"))) "ab"))
              (list (lit-node "b") (lit-node "a")))
(check-expect (star-foldl (lambda (ast acc) (cons ast acc))
                          empty
                          (lex (star (seq 'ab (lit "a") (lit "b"))) "abab"))
              (list (seq-node 'ab (lit-node "a") (lit-node "b"))
                    (seq-node 'ab (lit-node "a") (lit-node "b"))))

; flatten-ast tests
(check-expect (flatten-ast (lex (emp) ""))
              "")
(check-expect (flatten-ast (lex (lit ".") "."))
              ".")
(check-expect (flatten-ast (lex (wrap '+ (lit "+") (lit "!") (lit "+")) "+!+"))
              "!")
(check-expect (flatten-ast (lex (seq* 'ab (lit "a") (lit "b")) "ab"))
              "ab")

(test)

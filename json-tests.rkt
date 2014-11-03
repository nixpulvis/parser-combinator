#lang racket
(require test-engine/racket-tests)
(require "lex.rkt")
(require "json.rkt")

; parse-json-string tests
(check-expect (parse-json-string (lex STRING "\"Hello World\""))
              "Hello World")
(check-expect (parse-json-string (lex STRING "\"\""))
              "")
(check-expect (parse-json-string (lex STRING "\"-_.,+- \""))
              "-_.,+- ")
(check-expect (parse-json-string (lex STRING "\"(λ(x)(x x))\""))
              "(λ(x)(x x))")

; parse-json-number tests
(check-expect (parse-json-number (lex NUMBER "1"))
              1)
(check-expect (parse-json-number (lex NUMBER "-4"))
              -4)
(check-expect (parse-json-number (lex NUMBER "1337"))
              1337)
(check-within (parse-json-number (lex NUMBER "0.123"))
              0.123 0.0001)
(check-within (parse-json-number (lex NUMBER "123E2"))
              12300.0 0.0001)
(check-within (parse-json-number (lex NUMBER "123e-4"))
              0.0123 0.0001)
(check-within (parse-json-number (lex NUMBER "-123.456e-2"))
              -1.23456 0.0001)

; parse-json-pair tests
(check-expect (parse-json-pair (lex PAIR "\"\":\"\""))
              (make-json-pair "" ""))
(check-expect (parse-json-pair (lex PAIR "\"foo\":\"bar\""))
              (make-json-pair "foo" "bar"))
(check-expect (parse-json-pair (lex PAIR "\"foo\":123"))
              (make-json-pair "foo" 123))
(check-expect (parse-json-pair (lex PAIR "\"foo\" : 123"))
              (make-json-pair "foo" 123))
(check-expect (parse-json-pair (lex PAIR "\"foo\" \t: 123"))
              (make-json-pair "foo" 123))

; parse-json-array tests
(check-expect (parse-json-array (lex ARRAY "[]"))
              empty)
(check-expect (parse-json-array (lex ARRAY "[\"\"]"))
              (list ""))
(check-expect (parse-json-array (lex ARRAY "[\"\",123,\"hi\"]"))
              (list "" 123 "hi"))
(check-expect (parse-json-array (lex ARRAY "[1,2,3,4,5,6,7,8,9,10]"))
              (list 1 2 3 4 5 6 7 8 9 10))
(check-expect (parse-json-array (lex ARRAY "[\"\",123,[]]"))
              (list "" 123 empty))
(check-expect (parse-json-array (lex ARRAY "[\"\",123,[4,5,6]]"))
              (list "" 123 (list 4 5 6)))
(check-expect (parse-json-array (lex ARRAY " [\"\", 123, [4, 5, 6]]"))
              (list "" 123 (list 4 5 6)))

; parse-json-object tests
(check-expect (parse-json-object (lex OBJECT "{}"))
              empty)
(check-expect (parse-json-object (lex OBJECT "{\"foo\":\"bar\"}"))
              (list (make-json-pair "foo" "bar")))
(check-expect (parse-json-object (lex OBJECT "{\"a\":\"b\",\"c\":\"d\"}"))
              (list (make-json-pair "a" "b") (make-json-pair "c" "d")))
(check-expect (parse-json-object (lex OBJECT "{\"a\":{\"b\":\"c\"}}"))
              (list (make-json-pair "a" (list (make-json-pair "b" "c")))))
(check-expect (parse-json-object (lex OBJECT "{\"a\":1,\"b\":[],\"c\":{}}"))
              (list (make-json-pair "a" 1)
                    (make-json-pair "b" empty)
                    (make-json-pair "c" empty)))
(check-expect (parse-json-object (lex OBJECT "{\"a\" : 1, \"b\" : [], \"c\" : {}}"))
              (list (make-json-pair "a" 1)
                    (make-json-pair "b" empty)
                    (make-json-pair "c" empty)))

; parse-json-value tests
(check-expect (parse-json-value (lex VALUE "1"))
              1)
(check-expect (parse-json-value (lex VALUE "\"Hello World\""))
              "Hello World")
(check-expect (parse-json-value (lex VALUE "true"))
              true)
(check-expect (parse-json-value (lex VALUE "false"))
              false)
(check-expect (parse-json-value (lex VALUE "null"))
              'null)

; string->json tests
(define rjson-string1 "\"\"")
(define rjson-string2 "\"Hello World!\"")
(define rjson-string3 "\"CamBot\"")
(define rjson-string4 "\"Please mind the jump.\"")
(define rjson-number1 "0")
(define rjson-number2 "3.141592")
(define rjson-number3 "2.5e-2")
(define rjson-array1 "[]")
(define rjson-array2 "[1,2,3,4,5]")
(define rjson-array3 "[1,\"hi\",false,{\"foo\":10}]")
(define rjson-array4 "[1,2,3,4]")
(define rjson-array5 "[{}]")
(define rjson-array6 "[[1,2,3],[4,5,6]]")
(define rjson-object1 "{}")
(define rjson-object2 "{\"something\":10}")
(define rjson-object3 "{\"foo\":1337,\"bar\":\"yo!\"}")
(define rjson-object4 "{\"life\":42,\"death\":666}")
(define rjson-object5 "{\"foo\":{\"bar\":1}}")
(define rjson-value1 "true")
(define rjson-value2 "false")
(define rjson-value3 "null")

(define json-string1  "")
(define json-string2  "Hello World!")
(define json-string3  "CamBot")
(define json-string4  "Please mind the jump.")
(define json-number1  0)
(define json-number2  #e3.141592)
(define json-number3  #e2.5e-2)
(define json-array1  empty)
(define json-array2  (list 1 2 3 4 5))
(define json-array3  (list 1 "hi" false (list (make-json-pair "foo" 10))))
(define json-array4  (list 1 2 3 4))
(define json-array5  (list empty))
(define json-array6  (list (list 1 2 3) (list 4 5 6)))
(define json-object1 empty)
(define json-object2 (list (make-json-pair "something" 10)))
(define json-object3 (list (make-json-pair "foo" 1337) (make-json-pair "bar" "yo!")))
(define json-object4 (list (make-json-pair "life" 42) (make-json-pair "death" 666)))
(define json-object5 (list (make-json-pair "foo" (list (make-json-pair "bar" 1)))))
(define json-value1  true)
(define json-value2  false)
(define json-value3  'null)

(check-expect (string->json rjson-string1) json-string1)
(check-expect (string->json rjson-string2) json-string2)
(check-expect (string->json rjson-string3) json-string3)
(check-expect (string->json rjson-string4) json-string4)
(check-expect (string->json rjson-number1) json-number1)
(check-expect (string->json rjson-number2) json-number2)
(check-expect (string->json rjson-number3) json-number3)
(check-expect (string->json rjson-array1) json-array1)
(check-expect (string->json rjson-array2) json-array2)
(check-expect (string->json rjson-array3) json-array3)
(check-expect (string->json rjson-array4) json-array4)
(check-expect (string->json rjson-array5) json-array5)
(check-expect (string->json rjson-array6) json-array6)
(check-expect (string->json rjson-object1) json-object1)
(check-expect (string->json rjson-object2) json-object2)
(check-expect (string->json rjson-object3) json-object3)
(check-expect (string->json rjson-object4) json-object4)
(check-expect (string->json rjson-object5) json-object5)
(check-expect (string->json rjson-value1) json-value1)
(check-expect (string->json rjson-value2) json-value2)
(check-expect (string->json rjson-value3) json-value3)

(check-expect (list? (string->json
#<<HERE
[
    "JSON Test Pattern pass1",
    {"object with 1 member":["array with 1 element"]},
    {},
    [],
    -42,
    true,
    false,
    null,
    {
        "integer": 1234567890,
        "real": -9876.543210,
        "e": 0.123456789e-12,
        "E": 1.234567890E+34,
        "":  23456789012E66,
        "zero": 0,
        "one": 1,
        "space": " ",
        "quote": "\"",
        "backslash": "\\",
        "controls": "\b\f\n\r\t",
        "slash": "/ & \/",
        "alpha": "abcdefghijklmnopqrstuvwyz",
        "ALPHA": "ABCDEFGHIJKLMNOPQRSTUVWYZ",
        "digit": "0123456789",
        "0123456789": "digit",
        "special": "`1~!@#$%^&*()_+-={':[,]}|;.</>?",
        "hex": "\u0123\u4567\u89AB\uCDEF\uabcd\uef4A",
        "true": true,
        "false": false,
        "null": null,
        "array":[  ],
        "object":{  },
        "address": "50 St. James Street",
        "url": "http://www.JSON.org/",
        "comment": "// /* <!-- --",
        "# -- --> */": " ",
        " s p a c e d " :[1,2 , 3

,

4 , 5        ,          6           ,7        ],"compact":[1,2,3,4,5,6,7],
        "jsontext": "{\"object with 1 member\":[\"array with 1 element\"]}",
        "quotes": "&#34; \u0022 %22 0x22 034 &#x22;",
        "\/\\\"\uCAFE\uBABE\uAB98\uFCDE\ubcda\uef4A\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?"
: "A key can be any string"
    },
    0.5 ,98.6
,
99.44
,

1066,
1e1,
0.1e1,
1e-1,
1e00,2e+00,2e-00
,"rosebud"]
HERE
)) true)

(check-expect (list? (string->json
#<<HERE
[[[[[[[[[[[[[[[[[[["Not too deep"]]]]]]]]]]]]]]]]]]]
HERE
)) true)

(check-expect (list? (string->json
#<<HERE
{
    "JSON Test Pattern pass3": {
        "The outermost value": "must be an object or array.",
        "In this test": "It is an object."
    }
}
HERE
)) true)


(test)
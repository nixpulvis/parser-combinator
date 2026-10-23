#lang racket
(require rackunit)
(require racket/runtime-path)
(require (only-in json string->jsexpr))
(require "../parse.rkt")
(require "../json.rkt")

; SPACE tests
(check-equal? (parse SPACE " \t\n\r ")
              " \t\n\r ")
(check-exn exn:fail? (λ () (parse SPACE "")))
(check-equal? (parse VALUE " \t[ 1 ,\n2 ]\r\n")
              (list 1 2))

; STRING tests
(check-equal? (parse STRING "\"Hello World\"")
              "Hello World")
(check-equal? (parse STRING "\"\"")
              "")
(check-equal? (parse STRING "\"-_.,+- \"")
              "-_.,+- ")
(check-equal? (parse STRING "\"(λ(x)(x x))\"")
              "(λ(x)(x x))")
(check-equal? (parse STRING "\"tab\\there\"")
              "tab\there")
(check-equal? (parse STRING "\"\\\" \\\\ \\/ \\b \\f \\n \\r \\t\"")
              "\" \\ / \b \f \n \r \t")
(check-equal? (parse STRING "\"\\u0041\\u00e9\\u4F60\"")
              "Aé你")
(check-equal? (parse STRING "\"\\u005C\\u0022\"")
              "\\\"")
(check-equal? (parse STRING "\"\\uD83D\\uDE00\"")
              "\U1F600")
(check-equal? (parse STRING "\"\\uDBFF\\uDFFF\"")
              "\U10FFFF")
(check-equal? (parse STRING "\"\\uD83D\"")
              "\uFFFD")
(check-equal? (parse STRING "\"\\uD83Dx\"")
              "\uFFFDx")
(check-equal? (parse STRING "\"\\uD83D\\u0041\"")
              "\uFFFDA")
(check-equal? (parse STRING "\"\\uDE00\\uD83D\"")
              "\uFFFD\uFFFD")
(check-equal? (parse STRING "\"\\uD83D\\uD83D\\uDE00\"")
              "\uFFFD\U1F600")
(check-exn exn:fail? (λ () (parse STRING "\"\\u12\"")))
; A `\u` escape with a non-hex digit, or cut off by the end of the input,
; fails to match rather than raising an error.
(check-equal? (STRING "\"\\u12G4\"")
              'error)
(check-equal? (STRING "\"\\u12")
              'error)
(check-exn exn:fail? (λ () (parse STRING "\"tab\there\"")))
(check-exn exn:fail? (λ () (parse STRING "\"nul\u0000\"")))
(check-exn exn:fail? (λ () (parse STRING "\"us\u001F\"")))
(check-exn exn:fail? (λ () (parse STRING "\"\\x\"")))

; NUMBER tests
(check-equal? (parse NUMBER "1")
              1)
(check-equal? (parse NUMBER "-4")
              -4)
(check-equal? (parse NUMBER "1337")
              1337)
(check-equal? (parse NUMBER "0.123")
              #e0.123)
(check-equal? (parse NUMBER "123E2")
              12300)
(check-equal? (parse NUMBER "123e-4")
              #e0.0123)
(check-equal? (parse NUMBER "-123.456e-2")
              #e-1.23456)
(check-equal? (parse NUMBER "1e5")
              100000)
; A number longer than 64 digits, read without adding them up one at a
; time, is still exact.
(check-equal? (parse NUMBER (make-string 1000 #\9))
              (sub1 (expt 10 1000)))
(check-exn exn:fail? (λ () (parse NUMBER "1e")))
(check-exn exn:fail? (λ () (parse NUMBER "1e+")))
(check-exn exn:fail? (λ () (parse NUMBER "1.")))
(check-exn exn:fail? (λ () (parse NUMBER "1.e5")))
(check-exn exn:fail? (λ () (string->json "1e")))
(check-exn exn:fail? (λ () (string->json "[1.]")))

; PAIR tests
(check-equal? (parse PAIR "\"\":\"\"")
              (cons '|| ""))
(check-equal? (parse PAIR "\"foo\":\"bar\"")
              (cons 'foo "bar"))
(check-equal? (parse PAIR "\"foo\":123")
              (cons 'foo 123))
(check-equal? (parse PAIR "\"foo\" : 123")
              (cons 'foo 123))
(check-equal? (parse PAIR "\"foo\" \t: 123")
              (cons 'foo 123))

; ARRAY tests
(check-equal? (parse ARRAY "[]")
              empty)
(check-equal? (parse ARRAY "[ ]")
              empty)
(check-equal? (parse ARRAY "[\"\"]")
              (list ""))
(check-equal? (parse ARRAY "[\"\",123,\"hi\"]")
              (list "" 123 "hi"))
(check-equal? (parse ARRAY "[1,2,3,4,5,6,7,8,9,10]")
              (list 1 2 3 4 5 6 7 8 9 10))
(check-equal? (parse ARRAY "[\"\",123,[]]")
              (list "" 123 empty))
(check-equal? (parse ARRAY "[\"\",123,[4,5,6]]")
              (list "" 123 (list 4 5 6)))
(check-equal? (parse ARRAY " [\"\", 123, [4, 5, 6]]")
              (list "" 123 (list 4 5 6)))
(check-exn exn:fail? (λ () (parse ARRAY "[1,]")))
(check-exn exn:fail? (λ () (parse ARRAY "[,1]")))

; OBJECT tests
(check-equal? (parse OBJECT "{}")
              (hasheq))
(check-equal? (parse OBJECT "{\"foo\":\"bar\"}")
              (hasheq 'foo "bar"))
(check-equal? (parse OBJECT "{\"a\":\"b\",\"c\":\"d\"}")
              (hasheq 'a "b" 'c "d"))
(check-equal? (parse OBJECT "{\"a\":{\"b\":\"c\"}}")
              (hasheq 'a (hasheq 'b "c")))
(check-equal? (parse OBJECT "{\"a\":1,\"b\":[],\"c\":{}}")
              (hasheq 'a 1 'b empty 'c (hasheq)))
(check-equal? (parse OBJECT "{\"a\" : 1, \"b\" : [], \"c\" : {}}")
              (hasheq 'a 1 'b empty 'c (hasheq)))
(check-equal? (parse OBJECT "{\"a\":1,\"b\":2,\"a\":3}")
              (hasheq 'a 3 'b 2))
(check-exn exn:fail? (λ () (parse OBJECT "{\"a\":1,}")))

; VALUE tests
(check-equal? (parse VALUE "1")
              1)
(check-equal? (parse VALUE "\"Hello World\"")
              "Hello World")
(check-equal? (parse VALUE "true")
              true)
(check-equal? (parse VALUE "false")
              false)
(check-equal? (parse VALUE "null")
              'null)
(check-equal? (parse VALUE " \n null\t")
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
(define json-array3  (list 1 "hi" false (hasheq 'foo 10)))
(define json-array4  (list 1 2 3 4))
(define json-array5  (list (hasheq)))
(define json-array6  (list (list 1 2 3) (list 4 5 6)))
(define json-object1 (hasheq))
(define json-object2 (hasheq 'something 10))
(define json-object3 (hasheq 'foo 1337 'bar "yo!"))
(define json-object4 (hasheq 'life 42 'death 666))
(define json-object5 (hasheq 'foo (hasheq 'bar 1)))
(define json-value1  true)
(define json-value2  false)
(define json-value3  'null)

(check-equal? (string->json rjson-string1) json-string1)
(check-equal? (string->json rjson-string2) json-string2)
(check-equal? (string->json rjson-string3) json-string3)
(check-equal? (string->json rjson-string4) json-string4)
(check-equal? (string->json rjson-number1) json-number1)
(check-equal? (string->json rjson-number2) json-number2)
(check-equal? (string->json rjson-number3) json-number3)
(check-equal? (string->json rjson-array1) json-array1)
(check-equal? (string->json rjson-array2) json-array2)
(check-equal? (string->json rjson-array3) json-array3)
(check-equal? (string->json rjson-array4) json-array4)
(check-equal? (string->json rjson-array5) json-array5)
(check-equal? (string->json rjson-array6) json-array6)
(check-equal? (string->json rjson-object1) json-object1)
(check-equal? (string->json rjson-object2) json-object2)
(check-equal? (string->json rjson-object3) json-object3)
(check-equal? (string->json rjson-object4) json-object4)
(check-equal? (string->json rjson-object5) json-object5)
(check-equal? (string->json rjson-value1) json-value1)
(check-equal? (string->json rjson-value2) json-value2)
(check-equal? (string->json rjson-value3) json-value3)

(check-true (list? (string->json
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
)))

(check-true (list? (string->json
#<<HERE
[[[[[[[[[[[[[[[[[[["Not too deep"]]]]]]]]]]]]]]]]]]]
HERE
)))

(check-true (hash? (string->json
#<<HERE
{
    "JSON Test Pattern pass3": {
        "The outermost value": "must be an object or array.",
        "In this test": "It is an object."
    }
}
HERE
)))

; Fixture tests
; Each file in fixtures/ is real-world JSON from nativejson-benchmark, and
; parses to the same value as with Racket's own parser, string->jsexpr.
(define-runtime-path fixtures "fixtures")

; String -> Boolean
; Whether string->json and string->jsexpr agree on the fixture `name`.
(define (same-as-racket? name)
  (let ([text (file->string (build-path fixtures name))])
    (same-json? (string->json text) (string->jsexpr text))))

; JSON JSON -> Boolean
; Whether `a` and `b` are the same JSON. string->jsexpr makes a number with
; a fraction or exponent a flonum, where string->json makes it exact, so
; numbers are the same if they're the same as flonums.
(define (same-json? a b)
  (cond [(and (number? a) (number? b))
         (= (real->double-flonum a) (real->double-flonum b))]
        [(and (list? a) (list? b))
         (and (= (length a) (length b)) (andmap same-json? a b))]
        [(and (hash? a) (hash? b))
         (and (= (hash-count a) (hash-count b))
              (for/and ([(k v) (in-hash a)])
                (and (hash-has-key? b k) (same-json? v (hash-ref b k)))))]
        [else (equal? a b)]))

(check-true (same-as-racket? "canada.json"))
(check-true (same-as-racket? "citm_catalog.json"))
(check-true (same-as-racket? "twitter.json"))

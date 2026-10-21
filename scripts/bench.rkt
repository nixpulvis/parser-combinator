#!/usr/bin/env racket
#lang racket/base
; Benchmarks string->json against Racket's own JSON parser,
; string->jsexpr, and against string->json at other git refs, using the
; benchmark package.
;
;   scripts/bench.rkt [--trials N] [--sizes N,...] [--plot FILE] [ref ...]
;
; Each ref is a git commit, such as a branch, whose json.rkt is
; benchmarked from a temporary worktree. With no refs, the working tree is
; benchmarked.
(require benchmark
         json
         math/statistics
         plot
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/runtime-path
         racket/string
         racket/system)

(define-runtime-path root "..")

(define racket-json "racket json")

; Natural String String String String -> String
; Repeats `item`, separated by `sep`, between `open` and `close`, until
; the result is at least `n` characters long.
(define (repeat-to n open item sep close)
  (let loop ([count 1]
             [len (+ (string-length open) (string-length item) (string-length close))])
    (if (>= len n)
        (string-append open (string-join (make-list count item) sep) close)
        (loop (add1 count) (+ len (string-length sep) (string-length item))))))

; A Case is a (cons String [Natural -> String]), a name and a function
; that makes a JSON text of about the given size.
(define cases
  (list
   (cons "objects"
         (lambda (n)
           (repeat-to n "["
                      "{\"id\": 12, \"name\": \"item\", \"tags\": [1, 2.5, true, null]}"
                      ", " "]")))
   (cons "numbers"
         (lambda (n) (repeat-to n "[" "-12.5e3" "," "]")))
   (cons "text"
         (lambda (n) (string-append "\"" (make-string (max 0 (- n 2)) #\a) "\"")))
   (cons "escapes"
         (lambda (n) (repeat-to n "[" "\"caf\\u00e9\\n\\uD83D\\uDE00\"" "," "]")))
   (cons "nesting"
         (lambda (n)
           (let ([depth (quotient n 2)])
             (string-append (make-string depth #\[) (make-string depth #\])))))))

; String ... -> Void
; Runs git in this checkout.
(define (git . args)
  (unless (apply system* (find-executable-path "git") "-C" root args)
    (raise-user-error 'bench "git ~a failed" (string-join args " "))))

; [Listof String] [[Listof (cons String Path)] -> Any] -> Any
; Calls `proc` with a label and a checkout directory for each ref, using
; temporary worktrees that are removed afterwards. With no refs, the one
; checkout is this working tree.
(define (call-with-checkouts refs proc)
  (if (empty? refs)
      (proc (list (cons "working tree" root)))
      (let ([tmp (make-temporary-directory "bench~a")])
        (dynamic-wind
         void
         (lambda ()
           (proc (for/list ([ref (in-list refs)] [i (in-naturals)])
                   (let ([dir (build-path tmp (number->string i))])
                     (git "worktree" "add" "--quiet" "--detach" (path->string dir) ref)
                     (cons ref dir)))))
         (lambda ()
           (for ([dir (in-list (directory-list tmp #:build? #t))])
             (git "worktree" "remove" "--force" (path->string dir)))
           (delete-directory/files tmp))))))

; [Listof (cons String Path)] Natural [Listof Natural] (U Path-String #f) -> Void
(define (bench checkouts trials sizes plot-file-name)
  (let* ([parsers
          (append (for/list ([c (in-list checkouts)])
                    (cons (car c)
                          (dynamic-require (build-path (cdr c) "json.rkt") 'string->json)))
                  (list (cons racket-json string->jsexpr)))]
         ; Inputs are made before timing starts. A What is (list case size),
         ; and names the input `inputs` maps it to.
         [whats (for*/list ([c (in-list cases)] [n (in-list sizes)])
                  (list (car c) n))]
         [inputs (for/hash ([w (in-list whats)])
                   (values w ((cdr (assoc (car w) cases)) (cadr w))))]
         [what-name (lambda (w)
                      (format "~a ~a" (car w) (string-length (hash-ref inputs w))))]
         [parse-with (lambda (w label)
                       ((cdr (assoc label parsers)) (hash-ref inputs w)))]
         [results
          (run-benchmarks
           whats
           (list (map car parsers))
           parse-with
           ; Not timed: runs once before each benchmark's trials, to check
           ; that the parser accepts the input and to warm it up.
           #:build (lambda (w label)
                     (eprintf "~a, ~a\n" (what-name w) label)
                     (parse-with w label)
                     (collect-garbage))
           #:extract-time 'delta-time
           #:num-trials trials
           #:make-name what-name)])
    (print-table results)
    (when plot-file-name
      (parameterize ([plot-x-ticks no-ticks])
        (plot-file (render-benchmark-alts (list racket-json) results
                                          #:format-opts car)
                   plot-file-name
                   #:title "string->json"
                   #:x-label #f
                   #:y-label (format "time relative to ~a" racket-json)
                   #:width 1000))
      (printf "\nplot written to ~a\n" plot-file-name))))

; [Listof benchmark-result] -> Void
; Prints each result's mean and 95% confidence interval, computed as the
; plots from render-benchmark-alts compute theirs, and its mean relative
; to Racket's JSON parser on the same input.
(define (print-table results)
  (let* ([mean-of (lambda (r) (mean (benchmark-result-trial-times r)))]
         [baseline (for/hash ([r (in-list results)]
                              #:when (equal? (benchmark-result-opts r) (list racket-json)))
                     (values (benchmark-result-name r) (mean-of r)))]
         [rows
          (cons
           (list "input" "parser" "mean ms" "± ms" "vs json")
           (for/list ([r (in-list results)])
             (let* ([times (benchmark-result-trial-times r)]
                    [m (mean-of r)]
                    [base (hash-ref baseline (benchmark-result-name r))])
               (list (benchmark-result-name r)
                     (car (benchmark-result-opts r))
                     (~r m #:precision '(= 2))
                     (~r (* 1.96 (/ (stddev times) (sqrt (length times))))
                         #:precision '(= 2))
                     (if (zero? base)
                         "-"
                         (string-append (~r (/ m base) #:precision '(= 1)) "x"))))))]
         [widths (apply map (lambda cells (apply max (map string-length cells))) rows)])
    (newline)
    (for ([cells (in-list rows)])
      (displayln
       (string-join
        (for/list ([cell (in-list cells)]
                   [width (in-list widths)]
                   [align (in-list '(left left right right right))])
          (~a cell #:min-width width #:align align))
        "  ")))))

; String String -> Natural
(define (positive-integer flag str)
  (let ([n (string->number str)])
    (unless (exact-positive-integer? n)
      (raise-user-error 'bench "~a must be a positive integer, given ~s" flag str))
    n))

(module+ main
  (define trials 30)
  (define sizes '(4000 8000 16000))
  (define plot-file-name #f)
  (define refs
    (command-line
     #:once-each
     [("--trials") n "Times to run each benchmark (default 30)"
                   (set! trials (positive-integer "--trials" n))]
     [("--sizes") ns "Comma-separated input sizes in characters (default 4000,8000,16000)"
                  (set! sizes (for/list ([s (in-list (string-split ns ","))])
                                (positive-integer "--sizes" s)))]
     [("--plot") file "Also plot the times relative to Racket's JSON parser to file"
                 (set! plot-file-name file)]
     #:args refs
     refs))
  (call-with-checkouts refs
                       (lambda (checkouts) (bench checkouts trials sizes plot-file-name))))

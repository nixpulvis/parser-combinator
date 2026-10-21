#!/usr/bin/env racket
#lang racket/base
; Benchmarks string->json at git refs against each other, and optionally
; against Racket's own JSON parser, string->jsexpr, using the benchmark
; package.
;
;   scripts/bench.rkt [--racket] [--merge] [--trials N] [--sizes N,...] [--plot FILE] [ref ...]
;
; Each ref is a git commit, such as a branch, whose json.rkt is
; benchmarked from a temporary worktree. With no refs, the working tree is
; benchmarked. With --merge, so is a merge of each combination of the
; refs. With --racket, times are also shown relative to string->jsexpr's.
(require benchmark
         json
         math/statistics
         plot
         racket/cmdline
         racket/file
         racket/format
         racket/path
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

; Path-String String ... -> (values Boolean String)
; Runs git in `dir`, returning whether it succeeded and everything it
; printed.
(define (git? dir . args)
  (let* ([out (open-output-string)]
         [ok? (parameterize ([current-output-port out]
                             [current-error-port out])
                (apply system* (find-executable-path "git") "-C" dir args))])
    (values ok? (get-output-string out))))

; Path-String String ... -> String
; Runs git in `dir`, returning what it printed, or raising a user error if
; it fails.
(define (git dir . args)
  (let-values ([(ok? out) (apply git? dir args)])
    (unless ok?
      (raise-user-error 'bench "git ~a failed:\n~a" (string-join args " ") out))
    out))

; [Listof String] Boolean [[Listof (cons String Path)] -> Any] -> Any
; Calls `proc` with a label and a checkout directory for each ref, using
; temporary worktrees that are removed afterwards. With `merge?`, each
; combination of the refs is checked out instead, each ref on its own and
; every merge of two or more of them. With no refs, the one checkout is
; this working tree.
(define (call-with-checkouts refs merge? proc)
  (if (empty? refs)
      (proc (list (cons "working tree" root)))
      (let ([tmp (make-temporary-directory "bench~a")])
        (dynamic-wind
         void
         (lambda ()
           (proc (make-checkouts tmp (if merge?
                                         (for*/list ([k (in-range 1 (add1 (length refs)))]
                                                     [c (in-list (combinations refs k))])
                                           c)
                                         (map list refs)))))
         (lambda ()
           (for ([dir (in-list (directory-list tmp #:build? #t))])
             (git root "worktree" "remove" "--force" (path->string dir)))
           (delete-directory/files tmp))))))

; Path [Listof [Listof String]] -> [Listof (cons String Path)]
; Checks out each list of refs into a worktree in `tmp`, merging the rest
; of the refs into the first, and labels it with the refs joined by "+".
; Skips, with a note on stderr, any list that doesn't merge cleanly, and
; any whose code is the same as an earlier one's, which would only time
; the same code again.
(define (make-checkouts tmp ref-lists)
  (for/fold ([checkouts '()]
             [labels-by-tree (hash)]
             #:result (reverse checkouts))
            ([refs (in-list ref-lists)]
             [i (in-naturals)])
    (let ([label (string-join refs "+")]
          [dir (path->string (build-path tmp (number->string i)))])
      (git root "worktree" "add" "--quiet" "--detach" dir (first refs))
      (let* ([failure
              (for/or ([ref (in-list (rest refs))])
                ; The merge commits are thrown away with the worktree, so
                ; they don't need the user's identity.
                (let-values ([(ok? out) (git? dir "-c" "user.name=bench" "-c" "user.email=bench@localhost"
                                              "merge" "--quiet" "--no-edit" ref)])
                  (and (not ok?)
                       (begin (git? dir "merge" "--abort")
                              (format "merging ~a failed:\n~a" ref out)))))]
             [tree (and (not failure) (string-trim (git dir "rev-parse" "HEAD^{tree}")))]
             [same (and tree (hash-ref labels-by-tree tree #f))])
        (cond [(or failure same)
               (eprintf "skipping ~a: ~a\n" label (or failure (format "same code as ~a" same)))
               (git root "worktree" "remove" "--force" dir)
               (values checkouts labels-by-tree)]
              [else
               (values (cons (cons label (string->path dir)) checkouts)
                       (hash-set labels-by-tree tree label))])))))

; [Listof (cons String Path)] Boolean Natural [Listof Natural] (U Plot #f) -> Void
; Benchmarks string->json from each checkout, and Racket's string->jsexpr
; too if `racket?`, in which case times are also shown relative to it.
(define (bench checkouts racket? trials sizes plot)
  (let* ([parsers
          (append (for/list ([c (in-list checkouts)])
                    (cons (car c)
                          (dynamic-require (build-path (cdr c) "json.rkt") 'string->json)))
                  (if racket? (list (cons racket-json string->jsexpr)) '()))]
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
    (print-table results (> (length parsers) 1) racket?)
    (when plot
      (parameterize ([plot-x-ticks no-ticks])
        (plot-file (render-benchmark-alts (list (if racket? racket-json (car (first parsers))))
                                          results
                                          #:format-opts car
                                          #:normalize? racket?)
                   (car plot)
                   (cdr plot)
                   #:title "string->json"
                   #:x-label #f
                   #:y-label (if racket?
                                 (format "time relative to ~a" racket-json)
                                 "mean ms")
                   #:width 1000))
      (printf "\nplot written to ~a\n" (car plot)))))

; [Listof benchmark-result] Boolean Boolean -> Void
; Prints each result's mean and 95% confidence interval, computed as the
; plots from render-benchmark-alts compute theirs. With `parsers?`, it
; also names the parser of each result, and with `racket?`, it shows each
; mean relative to Racket's JSON parser's on the same input.
(define (print-table results parsers? racket?)
  (let* ([mean-of (lambda (r) (mean (benchmark-result-trial-times r)))]
         [racket-means (for/hash ([r (in-list results)]
                                  #:when (equal? (benchmark-result-opts r) (list racket-json)))
                         (values (benchmark-result-name r) (mean-of r)))]
         ; Each column is (list header alignment [benchmark-result -> String]).
         [columns
          (append
           (list (list "input" 'left benchmark-result-name))
           (if parsers?
               (list (list "parser" 'left (lambda (r) (car (benchmark-result-opts r)))))
               '())
           (list (list "mean ms" 'right (lambda (r) (~r (mean-of r) #:precision '(= 2))))
                 (list "± ms" 'right
                       (lambda (r)
                         (let ([times (benchmark-result-trial-times r)])
                           (~r (* 1.96 (/ (stddev times) (sqrt (length times))))
                               #:precision '(= 2))))))
           (if racket?
               (list (list (format "vs ~a" racket-json) 'right
                           (lambda (r)
                             (let ([base (hash-ref racket-means (benchmark-result-name r))])
                               (if (zero? base)
                                   "-"
                                   (string-append (~r (/ (mean-of r) base) #:precision '(= 1))
                                                  "x"))))))
               '()))]
         [rows (cons (map car columns)
                     (for/list ([r (in-list results)])
                       (for/list ([c (in-list columns)]) ((caddr c) r))))]
         [widths (apply map (lambda cells (apply max (map string-length cells))) rows)])
    (newline)
    (for ([cells (in-list rows)])
      (displayln
       (string-join
        (for/list ([cell (in-list cells)]
                   [width (in-list widths)]
                   [align (in-list (map cadr columns))])
          (~a cell #:min-width width #:align align))
        "  ")))))

; String String -> Natural
(define (positive-integer flag str)
  (let ([n (string->number str)])
    (unless (exact-positive-integer? n)
      (raise-user-error 'bench "~a must be a positive integer, given ~s" flag str))
    n))

; The extensions plot-file can write, and the format of each.
(define plot-formats
  '(("png" . png) ("jpg" . jpeg) ("jpeg" . jpeg) ("bmp" . bmp) ("xbm" . xbm)
    ("xpm" . xpm) ("pdf" . pdf) ("ps" . ps) ("svg" . svg)))

; A Plot is a (cons Path Symbol), a file to plot to and its format.

; String -> Plot
; The Plot for --plot's file. Raises a user error, so that a bad --plot
; fails before any benchmark runs, if plot-file can't write the file's
; extension, or if the file's directory doesn't exist.
(define (plot-target file)
  (let* ([path (path->complete-path file)]
         [ext (path-get-extension path)]
         [format (and ext
                      (assoc (string-downcase (substring (bytes->string/utf-8 ext) 1))
                             plot-formats))])
    (unless format
      (raise-user-error 'bench "--plot can't write ~a; use one of ~a"
                        file
                        (string-join (for/list ([f (in-list plot-formats)])
                                       (string-append "." (car f)))
                                     ", ")))
    (let-values ([(dir name must-be-dir?) (split-path path)])
      (unless (directory-exists? dir)
        (raise-user-error 'bench "--plot can't write ~a; ~a doesn't exist" file dir)))
    (cons path (cdr format))))

(module+ main
  (define racket? #f)
  (define merge? #f)
  (define trials 30)
  (define sizes '(4000 8000 16000))
  (define plot #f)
  (define refs
    (command-line
     #:once-each
     [("--racket") "Also benchmark Racket's own JSON parser, string->jsexpr, and compare against it"
                   (set! racket? #t)]
     [("--merge") "Also benchmark a merge of each combination of the refs, skipping any that conflict"
                  (set! merge? #t)]
     [("--trials") n "Times to run each benchmark (default 30)"
                   (set! trials (positive-integer "--trials" n))]
     [("--sizes") ns "Comma-separated input sizes in characters (default 4000,8000,16000)"
                  (set! sizes (for/list ([s (in-list (string-split ns ","))])
                                (positive-integer "--sizes" s)))]
     [("--plot") file "Also plot the times to file, such as a .png, .svg, or .pdf"
                 (set! plot (plot-target file))]
     #:args refs
     refs))
  (when (and merge? (< (length refs) 2))
    (raise-user-error 'bench "--merge needs at least two refs to merge"))
  (call-with-checkouts refs merge?
                       (lambda (checkouts)
                         (bench checkouts racket? trials sizes plot))))

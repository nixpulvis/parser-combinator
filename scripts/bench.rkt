#!/usr/bin/env racket
#lang racket/base
; Benchmarks string->json at git refs against each other, and optionally
; against Racket's own JSON parser, string->jsexpr, using the benchmark
; package.
;
;   scripts/bench.rkt [--racket] [--merge] [--vs PARSER]
;                     [--trials N] [--sizes N,...]
;                     [--plot FILE [--log-scale]] [ref ...]
;
; Each ref is a git commit, such as a branch, whose json.rkt is
; benchmarked from a temporary worktree. With no refs, the working tree is
; benchmarked. With --merge, so is a merge of each combination of the
; refs. With --racket, so is string->jsexpr, as the parser "racket". When
; there's more than one parser, times are also shown relative to the --vs
; parser's, or else to racket's, or else to the first ref's. The plot shows
; mean ms, unless --vs is given, when it shows times relative to its parser's.
(require benchmark
         math/statistics
         plot
         racket/format
         racket/list
         racket/math
         racket/string)

; A Parser is a (cons String [String -> Any]), a label and a function that
; parses a JSON text.

(module+ main
  (require json
           racket/cmdline
           racket/path)
  (define racket? #f)
  (define merge? #f)
  (define vs #f)
  (define trials 30)
  (define sizes '(4000 8000 16000))
  (define plot #f)
  (define log-scale? #f)
  ; String -> Path
  ; The path for --plot's file. Raises a user error, so that a bad --plot
  ; fails before any benchmark runs, if plot-file can't tell a format from
  ; the file's extension, or if the file's directory doesn't exist.
  (define (plot-path file)
    (let* ([path (path->complete-path file)]
           [ext (path-get-extension path)]
           [exts '(".png" ".jpg" ".jpeg" ".bmp" ".xbm" ".xpm" ".pdf" ".ps" ".svg")])
      (unless (and ext (member (string-downcase (bytes->string/utf-8 ext)) exts))
        (raise-user-error 'bench "--plot can't write ~a; use one of ~a"
                          file
                          (string-join exts ", ")))
      (let-values ([(dir name must-be-dir?) (split-path path)])
        (unless (directory-exists? dir)
          (raise-user-error 'bench "--plot can't write ~a; ~a doesn't exist" file dir)))
      path))
  (define refs
    (command-line
     #:once-each
     [("--racket") "Also benchmark Racket's own JSON parser, string->jsexpr, as the parser racket"
                   (set! racket? #t)]
     [("--merge") "Also benchmark a merge of each combination of the refs, skipping any that conflict"
                  (set! merge? #t)]
     [("--vs") parser "Show times relative to parser's, a ref, a merge such as a+b, or racket (default racket if benchmarked, else the first ref), and plot them relative to it too"
               (set! vs parser)]
     [("--trials") n "Times to run each benchmark (default 30)"
                   (set! trials (positive-integer "--trials" n))]
     [("--sizes") ns "Comma-separated input sizes in characters (default 4000,8000,16000)"
                  (set! sizes (for/list ([s (in-list (string-split ns ","))])
                                (positive-integer "--sizes" s)))]
     [("--plot") file "Also plot the times to file, such as a .png, .svg, or .pdf"
                 (set! plot (plot-path file))]
     [("--log-scale") "Plot the times on a log scale, so a much slower parser doesn't flatten the rest"
                      (set! log-scale? #t)]
     #:args refs
     refs))
  (when (and merge? (< (length refs) 2))
    (raise-user-error 'bench "--merge needs at least two refs to merge"))
  (when (and log-scale? (not plot))
    (raise-user-error 'bench "--log-scale needs --plot"))
  (call-with-checkouts
   refs merge?
   (lambda (checkouts)
     ; Racket's parser, if any, is first, to be the default --vs.
     (let ([parsers (append (if racket? (list (cons "racket" string->jsexpr)) '())
                            (for/list ([c (in-list checkouts)])
                              (cons (car c)
                                    (dynamic-require (build-path (cdr c) "json.rkt")
                                                     'string->json))))])
       (when (and vs (not (assoc vs parsers)))
         (raise-user-error 'bench "--vs ~a isn't a benchmarked parser; use one of ~a"
                           vs
                           (string-join (map car parsers) ", ")))
       ; With one parser, there's nothing to compare it to.
       (let ([baseline (and (> (length parsers) 1) (or vs (car (first parsers))))])
         (bench parsers baseline (and baseline vs #t) trials sizes plot log-scale?))))))

; [Listof Parser] (U String #f) Boolean Natural [Listof Natural] (U Path #f) Boolean -> Void
; Benchmarks each parser, printing the results, and plotting them too if
; there's a `plot`. With a `baseline`, the label of one of the parsers,
; times are also shown relative to its, and plotted relative to its if
; `plot-relative?`. With `log-scale?`, the plot's y-axis is a log scale.
(define (bench parsers baseline plot-relative? trials sizes plot log-scale?)
  (let* (; Inputs are made before timing starts. A What is (list case size),
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
    (print-table results baseline)
    (when plot
      (plot-results results (and plot-relative? baseline) plot log-scale?))))

; A Case is a (cons String [Natural -> String]), a name and a function
; that makes a JSON text of about the given size.
(define cases
  (let ()
    ; Natural String String String String -> String
    ; Repeats `item`, separated by `sep`, between `open` and `close`,
    ; until the result is at least `n` characters long.
    (define (repeat-to n open item sep close)
      (let loop ([count 1]
                 [len (+ (string-length open) (string-length item) (string-length close))])
        (if (>= len n)
            (string-append open (string-join (make-list count item) sep) close)
            (loop (add1 count) (+ len (string-length sep) (string-length item))))))
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
               (string-append (make-string depth #\[) (make-string depth #\]))))))))

; [Listof benchmark-result] (U String #f) -> Void
; Prints each result's mean and 95% confidence interval, computed as the
; plots from render-benchmark-alts compute theirs. With a `baseline`
; parser's label, it also names the parser of each result and shows each
; other parser's mean relative to the baseline's on the same input.
(define (print-table results baseline)
  ; Nonnegative-Real -> String
  ; Formats a ratio such as 1.5x, with two significant figures below 1x, so
  ; a parser 40 times faster isn't 0.0x.
  (define (format-ratio x)
    (string-append (~r x #:precision (list '= (if (< 0 x 1)
                                                  (- 1 (exact-floor (log x 10)))
                                                  1)))
                   "x"))
  (let* (; Each column is (list header alignment [benchmark-result -> String]).
         [columns
          (append
           (list (list "input" 'left benchmark-result-name))
           (if baseline
               (list (list "parser" 'left (lambda (r) (car (benchmark-result-opts r)))))
               '())
           (list (list "mean ms" 'right (lambda (r) (~r (mean-time r) #:precision '(= 2))))
                 (list "± ms" 'right
                       (lambda (r)
                         (let ([times (benchmark-result-trial-times r)])
                           (~r (* 1.96 (/ (stddev times) (sqrt (length times))))
                               #:precision '(= 2))))))
           (if baseline
               (let ([relative (relative-to results baseline)])
                 (list (list (format "vs ~a" baseline) 'right
                             (lambda (r)
                               (let ([x (and (not (equal? (benchmark-result-opts r) (list baseline)))
                                             (relative r))])
                                 (if x (format-ratio x) "-"))))))
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

; [Listof benchmark-result] (U String #f) Path Boolean -> Void
; Plots each result's mean, relative to the `baseline` parser's on the
; same input if there is one, on a log scale if `log-scale?`.
(define (plot-results results baseline plot log-scale?)
  ; A log scale keeps a parser many times slower than the others from
  ; flattening their bars. It can't reach 0, where the bars start, so it
  ; runs from half the smallest bar to twice the largest.
  (let* ([bars (filter-map (if baseline (relative-to results baseline) mean-time)
                           results)]
         [y-min (and log-scale? (/ (apply min bars) 2))]
         [y-max (and log-scale? (* (apply max bars) 2))])
    (parameterize ([plot-x-ticks no-ticks]
                   [plot-y-transform (if log-scale? log-transform (plot-y-transform))]
                   ; With more powers of 10 in range than #:number, log-ticks
                   ; labels only every other one, so it gets enough for all.
                   [plot-y-ticks (if log-scale?
                                     (log-ticks #:number (add1 (exact-ceiling (log (/ y-max y-min) 10))))
                                     (plot-y-ticks))]
                   ; Outside the plot, so it can't cover any bars.
                   [plot-legend-anchor 'outside-right-top])
      ; render-benchmark-alts needs a parser to normalize to even when it
      ; doesn't normalize, so without a baseline it gets the first result's.
      (plot-file (render-benchmark-alts (if baseline
                                            (list baseline)
                                            (benchmark-result-opts (first results)))
                                        results
                                        #:format-opts car
                                        #:normalize? (and baseline #t))
                 plot
                 #:title "string->json"
                 #:x-label #f
                 #:y-label (string-append
                            (if baseline
                                (format "time relative to ~a" baseline)
                                "mean ms")
                            (if log-scale? ", log scale" ""))
                 #:y-min y-min
                 #:y-max y-max
                 #:width 1200)))
  (printf "\nplot written to ~a\n" plot))

; [Listof benchmark-result] String -> [benchmark-result -> (U Real #f)]
; Maps each result to its mean relative to the mean of the `baseline`
; parser's result on the same input, or to #f if that mean is 0.
(define (relative-to results baseline)
  (let ([base-means (for/hash ([r (in-list results)]
                               #:when (equal? (benchmark-result-opts r) (list baseline)))
                      (values (benchmark-result-name r) (mean-time r)))])
    (lambda (r)
      (let ([base (hash-ref base-means (benchmark-result-name r))])
        (and (not (zero? base)) (/ (mean-time r) base))))))

; benchmark-result -> Real
(define (mean-time r)
  (mean (benchmark-result-trial-times r)))

; String String -> Natural
(define (positive-integer flag str)
  (let ([n (string->number str)])
    (unless (exact-positive-integer? n)
      (raise-user-error 'bench "~a must be a positive integer, given ~s" flag str))
    n))

; Checks out the refs to benchmark. A submodule keeps the git helpers out
; of the rest of the script, and must be declared before it's required.
(module git racket/base
  (require racket/file
           racket/list
           racket/runtime-path
           racket/string
           racket/system)
  (provide call-with-checkouts)

  (define-runtime-path root "..")

  ; [Listof String] Boolean [[Listof (cons String Path)] -> Any] -> Any
  ; Calls `proc` with a label and a checkout directory for each ref, using
  ; temporary worktrees that are removed afterwards. With `merge?`, each
  ; combination of the refs is checked out instead, each ref on its own and
  ; every merge of two or more of them. With no refs, the one checkout is
  ; this working tree.
  (define (call-with-checkouts refs merge? proc)
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

  ; Path-String String ... -> String
  ; Runs git in `dir`, returning what it printed, or raising a user error if
  ; it fails.
  (define (git dir . args)
    (let-values ([(ok? out) (apply git? dir args)])
      (unless ok?
        (raise-user-error 'bench "git ~a failed:\n~a" (string-join args " ") out))
      out))

  ; Path-String String ... -> (values Boolean String)
  ; Runs git in `dir`, returning whether it succeeded and everything it
  ; printed.
  (define (git? dir . args)
    (let* ([out (open-output-string)]
           [ok? (parameterize ([current-output-port out]
                               [current-error-port out])
                  (apply system* (find-executable-path "git") "-C" dir args))])
      (values ok? (get-output-string out)))))

(require 'git)

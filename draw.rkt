#lang racket
(require racket/draw "utilities.rkt" "configuration.rkt" "inout.rkt")
(require plot/no-gui)
(provide (all-defined-out))

(define frame (make-bitmap 1200 1200))

(define dc (new bitmap-dc% [bitmap frame]))

(define (plot-mean-dc csv-file delta)
  (define data (load-data csv-file))
  (define max-pay (* 5 (compound delta ROUNDS)))
  (define cap (function (lambda (x) max-pay) #:color "blue"))
  (define d (lines (pack-points data) #:y-min 0 #:y-max (+ 5 max-pay)))
  (plot/dc (list d cap)
           dc 0 0 1200 400))

(define (plot-tests rank-file make-reader data-point
rounds delta pie)
(define test-results
(render-characters-f rank-file make-reader data-point
                                     rounds delta pie))
(match-define (list result result-m) test-results)
(define (pack lst alpha y-max)
    (for/list ([i (in-list lst)]
               [j (in-list DARK-COLORS)]
               [k (in-list CHAR-LIST)]
               )
      (if (list? i)
          (points (pack-coors i) #:color j #:line-width 6 #:alpha alpha #:label k #:y-min 0 #:y-max y-max)
          (points (list (list 0 0)) #:color j #:line-width 6 #:label k))))
(define data (pack result .4 70))
  (define data-m (pack result-m .7 100))
  (plot/dc data
           dc
           0 400
           1200 400)
  (plot/dc data-m
           dc
           0 800
           1200 400))

(define (plot-bundle csv-file delta rank-file make-reader data-point rounds pie)
  (plot-mean-dc csv-file delta)
(plot-tests rank-file make-reader data-point
rounds delta pie))

(define (plot-bundles mean-list delta-list rank-list make-reader data-point rounds pie)
(for ([i (in-list mean-list)]
[j (in-list delta-list)]
[k (in-list rank-list)])
(send dc erase)
(plot-bundle i j k make-reader data-point rounds pie)
(send frame save-file (string-append (generate-file-name delta TESTS) ".png") 'png)))

(define cluster "/home/linhchi.nguyen/run86/")
(define MEAN-FILE (list (string-append cluster "rank0")
(string-append cluster "mean20")
(string-append cluster "mean40")
(string-append cluster "mean60")
(string-append cluster "mean70")
(string-append cluster "mean80")
(string-append cluster "mean85")
(string-append cluster "mean90")
(string-append cluster "mean95")
(string-append cluster "mean99")))
(define RANK-FILE (list (string-append cluster "rank0")
(string-append cluster "rank20")
(string-append cluster "rank40")
(string-append cluster "rank60")
(string-append cluster "rank70")
(string-append cluster "rank80")
(string-append cluster "rank85")
(string-append cluster "rank90")
(string-append cluster "rank95")
(string-append cluster "rank99")))
(define DELTA-LIST (list 0 .2 .4 .6 .7 .8 .85 .9 .95 .99))
(define JUMP 100) ;; data point

(define (draws)
(plot-bundles MEAN-FILE DELTA-LIST RANK-FILE make-automaton-csv-reader JUMP ROUNDS PIE))

(module+ main (draws))

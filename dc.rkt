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
          (points (pack-coors i) #:color j #:line-width 7 #:alpha alpha #:label k #:y-min 0 #:y-max y-max)
          (points (list (list 0 0)) #:color j #:line-width 7 #:label k))))
(define data (pack result .4 80))
  (define data-m (pack result-m .7 100))
  (plot/dc data
           dc
           0 400
           1200 400)
  (plot/dc data-m
           dc
           0 800
           1200 400))

(define (plot-bundle csv-file delta rank-file make-reader data-point)
  (plot-mean-dc csv-file delta)
(plot-tests rank-file make-reader data-point
ROUNDS delta PIE))

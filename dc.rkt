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




`(define (plot-bundle mean result result-m name-list color-list)
  (define c (load-data csv-file))
  (define d (series->lines c))
  (define max-pay (* 5 (compound DELTA ROUNDS)))
  (define ceiling (function (lambda (x) max-pay) #:color "blue"))
  (plot/dc (list d ceiling) dc 0 0 1200 400)
  (define (pack lst)
    (for/list ([i (in-list lst)]
               [j (in-list color-list)]
               [k (in-list name-list)]
               )
      (if (list? i)
          (points (pack-coors i) #:color j #:line-width 5 #:alpha .4 #:label k)
          (points (list (list 0 0)) #:color j #:line-width 5 #:label k))))
  (define data (pack result))
  (define data-m (pack result-m))
  (plot/dc data
           dc
           0 400
           1200 800)
  (plot/dc data-m
           dc
           0 800
           1200 1200)
  )

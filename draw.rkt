#lang racket
(require racket/draw "utilities.rkt" "configuration.rkt" "inout.rkt")
(require plot/no-gui)
(provide (all-defined-out))

(define frame (make-bitmap 1200 1200))

(define dc (new bitmap-dc% [bitmap frame]))

(define (draw-mean means delta)
  (define max-pay (* 5 (compound delta ROUNDS)))
  (define cap (function (lambda (x) max-pay) #:color "blue"))
  (define d (lines (pack-points means) #:y-min 0 #:y-max (+ 5 max-pay)))
(define title (print-configuration STATE# PIE "discount method" delta))
  (plot/dc (list d cap) #:title title
           dc 0 0 1200 400))

(define (draw-mean-f csv-file delta)
(define data (load-data csv-file))
;(define d (take-even data))
(draw-mean data delta))

(define (draw-chars result result-m)
(define (pack lst alpha y-min y-max)
    (for/list ([i (in-list lst)]
               [j (in-list DARK-COLORS)]
               [k (in-list CHAR-LIST)]
               )
               (points (pack-coors i) #:color j #:size 4 #:line-width 4 #:alpha alpha #:label k #:y-min y-min #:y-max y-max)))
;; for newer simulation, the threshold decreases so there are more autos
;; you should lower the opacity (alpha) and the adjust the capture frame       
(define data (pack result .3 0 100)) ; .3 0 100
  (define data-m (pack result-m .5 20 120)) ; .5 20 120 
  (plot/dc data
           dc
           0 400
           1200 400)
  (plot/dc data-m
           dc
           0 800
           1200 400))

(define (draw-chars-f rank-file make-reader data-point
rounds delta pie)
(define test-results
(render-characters-f rank-file make-reader data-point
                                     rounds delta pie))
(apply draw-chars test-results))

(define (draw-bundle means char-test char-test-m delta filename)
(draw-mean means delta)
(draw-chars char-test char-test-m)
(send frame save-file filename 'png))


(define (draw-bundle-f csv-file delta rank-file make-reader data-point rounds pie filename)
(send dc erase)
  (draw-mean-f csv-file delta)
(draw-chars-f rank-file make-reader data-point
rounds delta pie)
(send frame save-file filename 'png))

(define (draw-bundles-f mean-list delta-list rank-list make-reader data-point rounds pie)
(for ([i (in-list mean-list)]
[j (in-list delta-list)]
[k (in-list rank-list)])
(draw-bundle-f i j k make-reader data-point rounds pie (string-append (generate-file-name j TESTS) ".png"))))

(define (draw-bundles-f-pie mean-list pie-list rank-list make-reader data-point rounds delta)
(for ([i (in-list mean-list)]
[j (in-list pie-list)]
[k (in-list rank-list)])
(draw-bundle-f i delta k make-reader data-point rounds j (string-append (generate-file-name j TESTS) ".png"))))

(define cluster "/home/linhchi.nguyen/data/")
(define MEAN-FILE (list 
;(string-append cluster "mean0")
;(string-append cluster "mean20")
;(string-append cluster "30mean")
;(string-append cluster "mean40")
;(string-append cluster "mean60")
;(string-append cluster "mean70")
;(string-append cluster "mean80")
;(string-append cluster "mean85")
;(string-append cluster "mean90")
;(string-append cluster "mean95")
;(string-append cluster "mean99")
(string-append cluster "mean50")
(string-append cluster "mean100")
(string-append cluster "mean200")
(string-append cluster "mean300")
(string-append cluster "mean400")
(string-append cluster "mean450")
))
(define RANK-FILE (list 
;(string-append cluster "rank0")
;(string-append cluster "rank20")
;(string-append cluster "rank30")
;(string-append cluster "rank40")
;(string-append cluster "rank60")
;(string-append cluster "rank70")
;(string-append cluster "rank80")
;(string-append cluster "rank85")
;(string-append cluster "rank90")
;(string-append cluster "rank95")
;(string-append cluster "rank99")
(string-append cluster "rank50")
(string-append cluster "rank100")
(string-append cluster "rank200")
(string-append cluster "rank300")
(string-append cluster "rank400")
(string-append cluster "rank450")
))
(define DELTA-LIST (list 0 .2 .4 .6 .7 .8 .85 .9 .95 .99))
(define JUMP 20) ;; data point

(define (draws)
;(draw-mean-f "/home/linhchi.nguyen/data/mean30" .3))
(draw-bundle-f "/home/linhchi.nguyen/data2/99mean" .99 "/home/linhchi.nguyen/data2/99rank" make-automaton-csv-reader JUMP ROUNDS PIE "99tests.png"))
;(draw-bundles-f-pie MEAN-FILE PIES RANK-FILE make-automaton-csv-reader JUMP ROUNDS DELTA))

(module+ main (draws))

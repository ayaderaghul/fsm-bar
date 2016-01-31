#lang racket

(require "basic.rkt" "configuration.rkt" "utilities.rkt" "population.rkt")

;; ACROSS STATES#
(define (print-state state#)
  (print-configuration state# PIE "discount method" DELTA))

(define (states)
  (for ([i (in-list STATES#)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-title (print-state i))
    (define pic-name (generate-file-name i PIC))    
    (define mean-name (generate-file-name i MEAN))
    (define rank-name (generate-file-name i RANK))    
    (define datas (time (evolve (build-random-population N i) CYCLES SPEED ROUNDS DELTA PIE MUTATION mean-name rank-name)))
    ;(define ps (map first datas))
    (plot-payoffs datas DELTA pic-title pic-name)
    ))

(module+ main (states))

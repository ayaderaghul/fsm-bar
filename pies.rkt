#lang racket

(require "basic.rkt" "configuration.rkt" "utilities.rkt" "population.rkt")

;; ACROSS PIES
(define (print-pie pie)
  (print-configuration STATE# pie "discount method" DELTA))

(define (pies)
  (for ([i (in-list PIES)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-title (print-pie i))
    (define pic-name (generate-file-name PIC i))    
    (define mean-name (generate-file-name MEAN i))
    (define rank-name (generate-file-name RANK i))    
    (define datas (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS DELTA i MUTATION mean-name rank-name)))
    ;(define ps (map first datas))
    (plot-payoffs datas i pic-title pic-name)
    ))

(module+ main (pies))



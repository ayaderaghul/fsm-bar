#lang racket

(require "basic.rkt" "configuration.rkt" "utilities.rkt" "population.rkt")

;; ACROSS DELTAS: DISCOUNT FACTOR

(define (deltas)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-title (print-delta i "discount factor"))
    (define pic-name (generate-file-name PIC i))    
    (define mean-name (generate-file-name MEAN i))
    (define rank-name (generate-file-name RANK i))    
    (define datas (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS i PIE MUTATION mean-name rank-name)))
    (define ps (map first datas))
    (plot-payoffs ps i pic-title pic-name)
    ))

(module+ main (deltas))

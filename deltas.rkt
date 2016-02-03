#lang racket

(require "basic.rkt" "configuration.rkt" "utilities.rkt" "population.rkt")

;; ACROSS DELTAS: DISCOUNT FACTOR

(define (deltas)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-title (print-delta i "discount factor"))
    (define pic-name (generate-file-name i PIC))    
    (define mean-name (generate-file-name i MEAN))
    (define rank-name (generate-file-name i RANK))
    ;(define res-name (generate-file-name i RES)) 
(define char-name (generate-file-name i CHAR))    
    (define datas (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS i PIE MUTATION mean-name rank-name)))
    (define ps (map first datas))
    (plot-payoffs ps i pic-title pic-name)
    (define char-hash (map second datas))
(define type-list (render-characters char-hash))
(plot-point-types (string-append char-name ".png") type-list CHAR-LIST DARK-COLORS)
    ))

(module+ main (deltas))

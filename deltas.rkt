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
(define t-name (generate-file-name i T))    
    (define b-name (generate-file-name i B))
    (define f-name (generate-file-name i F))
    (define a-name (generate-file-name i A))    
    (define datas (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS i PIE MUTATION mean-name rank-name)))
    (define ps (map first datas))
    (plot-payoffs ps i pic-title pic-name)
    (define ts (map second datas))
(define bs (map third datas))
(define fs (map fourth datas))
(define as (map fifth datas))
  (plot-payoff ts "toughs" t-name)
(plot-payoff bs "bullys" b-name)
(plot-payoff fs "fairs" f-name)
(plot-payoff as "accoms" a-name)

    ))

(module+ main (deltas))

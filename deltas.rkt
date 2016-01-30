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
    ;(define res-name (generate-file-name RES i)) 
(define t-name (generate-file-name T i))    
    (define b-name (generate-file-name B i))
    (define f-name (generate-file-name F i))
    (define a-name (generate-file-name A i))    
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

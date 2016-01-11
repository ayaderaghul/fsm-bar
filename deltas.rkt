;;#! /usr/bin/env racket -tm
#lang racket

(provide (all-defined-out))
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt" plot "basic.rkt")
(plot-new-window? #t)

;; CONFIGURATION
(define DELTAS-C (list .99 .95 .9 .85 .8 .7 .6 .4 .2 0))
(define DELTAS-D (list 1 .95 .9 .85 .8 .7 .6 .4 .2 0))


;; MAIN
(define (evolve-delta evolve-function delta)
  (evolve-function (build-random-population N)
                   CYCLES SPEED ROUNDS delta MUTATION))

(define (main-c)
  (for ([i (in-list DELTAS-C)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (configuration-string i "continual probability"))
    (define data (time (evolve-delta evolve-c i)))
    (plot-payoff data 10.0 pic-name (generate-file-name PIC i))
    (out-mean (generate-file-name MEAN i) data)
    ))

(module+ mainc (main-c))

(define (main-d)
  (for ([i (in-list DELTAS-D)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (configuration-string i "discount factor"))
    (define data (time (evolve-delta evolve-d i)))
    (define max-pay (apply max data))
    (plot-payoff data (+ 5 max-pay) pic-name (generate-file-name PIC i))
    (out-mean (generate-file-name MEAN i) data)
    ))

(module+ maind (main-d))

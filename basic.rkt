;;#! /usr/bin/env racket -tm
#lang racket

(provide (all-defined-out))
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt" plot)
(plot-new-window? #t)

;; CONFIGURATION
;; change the directory of output file here

(define lab1-directory "/Users/linhchi.nguyen/Dropbox/fsm-bar/deltas/run2/")

(define MEAN "mean")
(define RANK "rank")
(define PIC "mean.png")
;; change the simulation settings here
(define N 100)
(define P (build-random-population N))
(define CYCLES 50000)
(define SPEED 15)
(define ROUNDS-PER-MATCH 300)
(define DELTA .95)
(define MUTATION 1)


;; MAIN
(define (main)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-name (configuration-string N SPEED ROUNDS-PER-MATCH DELTA))
  (define datas
    (time (evolve P CYCLES SPEED ROUNDS-PER-MATCH DELTA MUTATION)))
  (define ps (map first datas)) ; mean
  (define max-pay (apply max ps))
  (define ts (map second datas)) ; number of types
  (define rs (map third datas)) ; highest ranking in each cycles
  (define mean-types# (/ (apply + ts) CYCLES))
  (plot (list (simulation->lines ps))
        #:y-min 0.0 #:y-max (+ 5 max-pay) #:title pic-name #:out-file PIC
        #:width 1200)
  (plot (list (simulation->lines ts))
        #:y-min 0.0 #:y-max (+ 10 mean-types#) #:title "types#")
  (plot (list (simulation->lines rs))
        #:y-min 0.0 #:y-max N #:title "biggest")
  (out-mean MEAN ps)
  )

(define (evolve population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-c p3 mutation)
         (define ranking (rank p3))
         (define ranking-list (hash->list ranking))
         (out-rank RANK cycles ranking-list)
         (cons (list (relative-average pp 1)
                     (hash-count ranking)
                     (apply max (if (hash-empty? ranking) (list 0) (hash-values ranking))))
               (evolve p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(module+ five
  (main)
  (main)
  (main)
  (main)
  (main))

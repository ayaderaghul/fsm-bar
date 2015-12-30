;;#! /usr/bin/env racket -tm
#lang racket

(provide main)
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt" plot)
(plot-new-window? #t)

;; CONFIGURATION
;; change the directory of output file here
(define MEAN "mean")
(define RANK "rank")
(define PIC "mean.png")
(define RME "readme.txt")

(define N 100)
(define P (build-random-population N))
(define CYCLES 1000)
(define SPEED 15)
(define ROUNDS-PER-MATCH 20)
(define DELTA 1)
(define MUTATION 1)

(define (main)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (print-configuration RME (list "N" N "speed" SPEED "rounds per match" ROUNDS-PER-MATCH "delta" DELTA "mutation" MUTATION))
  (define datas
    (time (evolve P CYCLES SPEED ROUNDS-PER-MATCH DELTA MUTATION)))
  (define ps (map first datas)) ; mean
  (define ts (map second datas)) ; number of types
  (define rs (map third datas)) ; highest ranking in each cycles
  (define mean-types# (/ (apply + ts) CYCLES))
  (plot (list (simulation->lines ps)) #:y-min 0.0 #:y-max 10.0 #:title "mean" #:out-file PIC)
  (plot (list (simulation->lines ts)) #:y-min 0.0 #:y-max (+ 10 mean-types#) #:title "types#")
  (plot (list (simulation->lines rs)) #:y-min 0.0 #:y-max N #:title "biggest")
  (out-mean MEAN ps)
  )

(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

(define (evolve population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate* p3 mutation)
         (define ranking (rank p3))
         (define ranking-list (hash->list ranking))
         (out-rank RANK cycles ranking-list)
         (cons (list (relative-average pp rounds-per-match)
                     (hash-count ranking)
                     (apply max (if (hash-empty? ranking) (list 0) (hash-values ranking)))
                     )
               (evolve p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(module+ five
  (main)
  (main)
  (main)
  (main)
  (main))
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
(define PIC "meanplot")

(define N 100)
(define P (build-random-population N))
(define CYCLES 1000)
(define SPEED 15)
(define ROUNDS-PER-MATCH 20)
(define DELTAS (list 0 .2 .4 .6 .8 1))
(define MUTATION 1)

(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

(define (evolve-d population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate* p3 mutation)
         (define ranking-list (hash->list (rank p3)))
         (out-rank (generate-name RANK i) cycles ranking-list)
         (cons (relative-average pp rounds-per-match)
               (evolve-d p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(define (evolve-delta delta)
  (evolve-d P CYCLES SPEED ROUNDS-PER-MATCH delta MUTATION))

(define (main)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define datas (time (evolve-delta i)))
    (plot (list (simulation->lines datas)) #:y-min 0.0 #:y-max 10.0 #:title "mean" #:out-file (generate-name PIC i)
          )
    (out-mean (generate-name MEAN i) datas)
    ))

(define (delta->string delta)
  (string-trim (number->string (* 10 delta)) ".0"))
(define (generate-name prefix delta)
  (string-append prefix (delta->string delta)))

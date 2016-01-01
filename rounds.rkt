;;#! /usr/bin/env racket -tm
#lang racket

(provide (all-defined-out))
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt" plot)
(plot-new-window? #t)

;; CONFIGURATION

(define lab1-directory "/Users/linhchi.nguyen/Dropbox/fsm-bar/deltas/run2/")

;; change the directory of output file here
(define MEAN "mean")
(define RANK "rank")
(define PIC "meanplot")

(define N 100)
(define P (build-random-population N))
(define CYCLES 100)
(define SPEED 15)
(define ROUNDS-PER-MATCH (list 1 5 10 15 20 50))
(define DELTA 1)
(define MUTATION 1)

;; UTILITIES
(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))
(define (generate-file-name prefix rounds)
  (string-append prefix (number->string rounds)))

;; MAIN
(define (evolve-r population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate* p3 mutation)
         (define ranking-list (hash->list (rank p3)))
         (out-rank (generate-file-name RANK rounds-per-match) cycles ranking-list)
         (cons (relative-average pp rounds-per-match)
               (evolve-r p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(define (evolve-rounds rounds)
  (evolve-r P CYCLES SPEED rounds DELTA MUTATION))

(define (main)
  (for ([i (in-list ROUNDS-PER-MATCH)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
	(define pic-name (configuration-string N SPEED i DELTA))
    (define datas (time (evolve-rounds i)))
    (define max-pay (apply max datas))
    (plot (list (simulation->lines datas))
          #:y-min 0.0 #:y-max (+ 3 max-pay) #:title pic-name
          #:out-file (string-append (generate-file-name PIC i) ".png")
          )
    (out-mean (generate-file-name MEAN i) datas)
    ))

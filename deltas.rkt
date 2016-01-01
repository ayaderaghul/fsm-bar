;;#! /usr/bin/env racket -tm
#lang racket

(provide (all-defined-out))
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt" plot)
(plot-new-window? #t)

;; CONFIGURATION
;; change the directory of output file here
(define MEAN "/Users/linhchi.nguyen/Dropbox/fsm-bar/deltas/run2/mean")
(define RANK "/Users/linhchi.nguyen/Dropbox/fsm-bar/deltas/run2/rank")
(define PIC "/Users/linhchi.nguyen/Dropbox/fsm-bar/deltas/run2/meanplot")

(define N 200)
(define P (build-random-population N))
(define CYCLES 100000)
(define SPEED 30)
(define ROUNDS-PER-MATCH 15)
(define DELTAS (list 0 .3 .6 .7 .8 .9 .95 1))
(define MUTATION 2)

;; UTILITIES
(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))
(define (delta->string delta)
  (string-trim (number->string (* 100 delta)) ".0"))
(define (generate-file-name prefix delta)
  (string-append prefix (delta->string delta)))

;; MAIN
(define (evolve-d population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate* p3 mutation)
         (define ranking-list (hash->list (rank p3)))
         (out-rank (generate-file-name RANK delta) cycles ranking-list)
         (cons (relative-average pp rounds-per-match)
               (evolve-d p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(define (evolve-delta delta)
  (evolve-d P CYCLES SPEED ROUNDS-PER-MATCH delta MUTATION))

(define (main)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
	(define pic-name (configuration-string N SPEED ROUNDS-PER-MATCH i))
    (define datas (time (evolve-delta i)))
    (define max-pay (apply max datas))
    (plot (list (simulation->lines datas))
          #:y-min 0.0 #:y-max (+ 3 max-pay) #:title pic-name
          #:out-file (string-append (generate-file-name PIC i) ".png")
          )
    (out-mean (generate-file-name MEAN i) datas)
    ))


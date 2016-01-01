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

(define Ns (list 100 200 300 500 1000))
(define CYCLES 10)
(define SPEEDs (list 15 30 45 75 150))
(define ROUNDS-PER-MATCH 15)
(define DELTA 1)
(define MUTATIONs (list 1 2 3 5 10))

;; UTILITIES
(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))
(define (generate-file-name prefix n)
  (string-append prefix (number->string n)))

;; MAIN
(define (evolve-N population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate* p3 mutation)
         (define ranking-list (hash->list (rank p3)))
         (out-rank (generate-file-name RANK (vector-length (car population))) cycles ranking-list)
         (cons (relative-average pp rounds-per-match)
               (evolve-N p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(define (evolve-Ns n s m)
  (define P (build-random-population n))
  (evolve-N P CYCLES s ROUNDS-PER-MATCH DELTA m))

(define (main)
  (for ([i (in-list Ns)]
        [s (in-list SPEEDs)]
        [m (in-list MUTATIONs)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (configuration-string i s ROUNDS-PER-MATCH DELTA))
    (define datas (time (evolve-Ns i s m)))
    (define max-pay (apply max datas))
    (plot (list (simulation->lines datas))
          #:y-min 0.0 #:y-max (+ 3 max-pay) #:title pic-name
          #:out-file (string-append (generate-file-name PIC i) ".png")
          )
    (out-mean (generate-file-name MEAN i) datas)
    ))

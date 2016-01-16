;;#! /usr/bin/env racket -tm
#lang racket

(provide (all-defined-out))
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt" plot/no-gui)

;; CONFIGURATION
;; change the directory of output file here
(define lab1 "/Users/linhchi.nguyen/Dropbox/fsm-bar/grand/deltas/d/run3/")
(define disa "C:/Documents and Settings/linhchi.nguyen/My Documents/Dropbox/fsm-bar/grand/deltas/c/run1/")

(define MEAN (string-append "" "mean"))
(define RANK (string-append "" "rank"))
(define PIC (string-append "" "meanplot"))

;; change the simulation settings here
(define N 100)
(define CYCLES 500000)
(define SPEED 15)
(define ROUNDS 100)
(define DELTA .95)
(define MUTATION 1)

;; UTILITIES
(define (configuration-string delta method)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, gamma = ~a, ~a: delta = ~a"
   N SPEED ROUNDS MAX-STATES# GAMMA method delta))
(define (plot-payoff lst y-max title file-name)
  (plot-file (simulation->lines lst) file-name 'png
        #:y-min 0.0 #:y-max y-max #:title title
                #:width 1200))

(define (delta->string delta)
  (string-trim (number->string (* 100 delta)) ".0"))
(define (generate-file-name prefix delta)
  (string-append prefix (delta->string delta)))

;; CONTINUAL PROBABILITY
(define (run-c)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-name (configuration-string DELTA "continual probability"))
  (define data
    (time (evolve-c (build-random-population N)
                    CYCLES SPEED ROUNDS DELTA MUTATION)))
  (plot-payoff data 10.0 pic-name PIC)
  (out-mean MEAN data)
  )

(define (evolve-c population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up-c population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-c p3 mutation)
         ;;(define ranking (rank p3))
         ;;(define ranking-list (hash->list ranking))
         ;;(out-rank (generate-file-name RANK delta) cycles ranking-list)
         (cons ;;(list
                (relative-average pp 1)
                ;;(hash-count ranking)
                ;;(apply max (if (hash-empty? ranking) (list 0) (hash-values ranking))))
                (evolve-c p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(module+ runc (run-c))

;; DISCOUNT FACTOR
(define (run-d)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-name (configuration-string DELTA "discount factor"))
  (define datas
    (time (evolve-d (build-random-population N)
                    CYCLES SPEED ROUNDS DELTA MUTATION)))
  (define ps (map first datas))
  (define max-pay (apply max ps))
  (define as (map second datas))
  ;;(define max-a (apply max as))
  (plot-payoff ps (+ 5 max-pay) pic-name PIC)
  ;;(plot (simulation->lines as) #:width 1200)
  (out-mean MEAN ps)
  )

(define (evolve-d population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up-d population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-c p3 mutation)
         (define ranking (rank* p3))
         (define ranking-list (hash->list ranking))
         (define sample-auto
           (if (empty? ranking-list) 0 (car (first ranking-list))))
         ;;(define a-rate (accommodating sample-auto))
         ;;(out-rank (generate-file-name RANK delta) cycles ranking-list)
         (cons (relative-average pp 1) 
                     ;;(hash-count ranking)
                     ;;(apply max (if (hash-empty? ranking) (list 0) (hash-values ranking))))
               (evolve-d p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(module+ rund
  (run-d))

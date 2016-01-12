;;#! /usr/bin/env racket -tm
#lang racket

(provide (all-defined-out))
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt" plot)
(plot-new-window? #t)

;; CONFIGURATION
;; change the directory of output file here
(define lab1-dir "/Users/linhchi.nguyen/Dropbox/fsm-bar/grand/deltas/")
(define disa-lab "C:/Documents and Settings/linhchi.nguyen/My Documents/Dropbox/fsm-bar/grand/deltas/d/run1/")
(define MEAN (string-append disa-lab "mean"))
(define RANK (string-append disa-lab "rank"))
(define PIC (string-append disa-lab "meanplot"))

;; change the simulation settings here
(define N 100)
(define CYCLES 1000000)
(define SPEED 15)
(define ROUNDS 300)
(define DELTA .99)
(define MUTATION 1)

;; UTILITIES
(define (configuration-string delta method)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, gamma = ~a, ~a: delta = ~a"
   N SPEED ROUNDS MAX-STATES# GAMMA method delta))
(define (plot-payoff lst y-max title file-name)
  (plot (list (simulation->lines lst))
        #:y-min 0.0 #:y-max y-max #:title title
        #:out-file (string-append file-name ".png")
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
  (define data
    (time (evolve-d (build-random-population N)
                    CYCLES SPEED ROUNDS DELTA MUTATION)))
  (define max-pay (apply max data))
  (plot-payoff data (+ 5 max-pay) pic-name PIC)
  (out-mean MEAN data)
  )

(define (evolve-d population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up-d population rounds-per-match delta))
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
               (evolve-d p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(module+ rund
  (run-d))

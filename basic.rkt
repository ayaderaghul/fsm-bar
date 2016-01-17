;;#! /usr/bin/env racket -tm
#lang racket

(provide (all-defined-out))
(require "configuration.rkt" "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt" plot/no-gui)

;; UTILITIES
(define (configuration-string delta method)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, gamma = ~a, ~a: delta = ~a"
   N SPEED ROUNDS MAX-STATES# GAMMA method delta))
(define (plot-payoff lst y-max title file-name) ;; for the continual method (no ceiling)
  (plot-file (simulation->lines lst) file-name 'png
        #:y-min 0.0 #:y-max y-max #:title title
                #:width 1200))

;; to calculate the compound rate of payoff
(define (compound d r) (foldl (lambda (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))

(define (plot-payoffs lst delta title file-name)
(define m-pay (* 5 (compound delta ROUNDS)))
(define ceiling (function (lambda (x) m-pay) #:color "blue"))
  (plot-file (list (simulation->lines lst) ceiling) file-name 'png
        #:y-min 0.0 #:y-max (+ 10 m-pay) #:title title
                #:width 1200))

(define (delta->string delta)
  (string-trim (number->string (* 100 delta)) ".0"))
(define (generate-file-name prefix delta)
  (string-append prefix (delta->string delta)))

;; ACROSS DELTAS: DISCOUNT FACTOR
(define (run)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-name (configuration-string DELTA "discount factor"))
(define rank-name (generate-file-name RANK DELTA))
  (define datas
    (time (evolve (build-random-population N) CYCLES SPEED ROUNDS DELTA MUTATION rank-name)))
  ;;(define ps (map first datas))
  ;;(define as (map second datas))
  ;;(define max-a (apply max as))
  (plot-payoffs datas DELTA pic-name PIC)
  ;;(plot (simulation->lines as) #:width 1200)
  (out-mean MEAN datas)
  )

(define (evolve population cycles speed rounds-per-match delta mutation rank-file)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up-d population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-c p3 mutation)
         (and (zero? (modulo cycles DATA-POINT))
              ;;(define sample-auto
              ;; (if (empty? ranking-list) 0 (car (first ranking-list))))
              ;;(define a-rate (accommodating sample-auto))
              (out-rank rank-file cycles
                        (hash->list (rank p3))))
         (cons (relative-average pp 1)
               ;;(hash-count ranking)
               ;;(apply max (if (hash-empty? ranking) (list 0) (hash-values ranking))))
               (evolve p3 (- cycles 1) speed rounds-per-match delta mutation rank-file))]))

(module+ rund
  (run))

(define (main)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (configuration-string i "discount factor"))
(define rank-name (generate-file-name RANK i))
    (define data (time (evolve (build-random-population N) CYCLES SPEED ROUNDS i MUTATION rank-name)))
    (plot-payoffs data i pic-name (generate-file-name PIC i))
    (out-mean (generate-file-name MEAN i) data)
    ))

;; ACROSS GAMMA
(define (print-configuration gamma)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, gamma = ~a, ~a: delta = ~a"
   N SPEED ROUNDS MAX-STATES# gamma "discount factor" DELTA))
(define (pies)
  (for ([i (in-list GAMMAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (print-configuration i))
(define rank-name (generate-file-name RANK i))
    (define PAYOFF-TABLE
      (vector (vector (cons i i) (cons i 5) (cons i (- 10 i)))
              (vector (cons 5 i) (cons 5 5) (cons 0 0))
              (vector (cons (- 10 i) i) (cons 0 0) (cons 0 0))))
    (define data (time (evolve (build-random-population N) CYCLES SPEED ROUNDS DELTA MUTATION rank-name)))
    (plot-payoffs data DELTA pic-name (generate-file-name PIC i))
    (out-mean (generate-file-name MEAN i) data)
    ))
(module+ pies (pies))
;; ACROSS STATES#
(define (print-title states#)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, gamma = ~a, ~a: delta = ~a"
   N SPEED ROUNDS states# GAMMA "discount factor" DELTA))
(define (states#)
  (for ([i (in-list STATES#)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (print-title i))
(define rank-name (generate-file-name RANK i))
    (define MAX-STATES# i)
    (define data (time (evolve (build-random-population N) CYCLES SPEED ROUNDS DELTA MUTATION rank-name)))
    (plot-payoffs data DELTA pic-name (generate-file-name PIC i))
    (out-mean (generate-file-name MEAN i) data)
    ))
(module+ states# (states#))


;; CONTINUAL PROBABILITY
(define (run-c)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-name (configuration-string DELTA "continual probability"))
  (define data
    (time (evolve-c (build-random-population N)
                    CYCLES SPEED ROUNDS DELTA MUTATION)))
  (plot-payoff data DELTA 10.0 pic-name PIC)
  ;;(out-mean MEAN data)
  )

(module+ runc (run-c))

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

(define (main-c)
  (for ([i (in-list DELTAS-C)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (configuration-string i "continual probability"))
    (define data (time (evolve-c (build-random-population N) CYCLES SPEED ROUNDS i MUTATION)))
    (plot-payoff data 10.0 pic-name (generate-file-name PIC i))
    ;;(out-mean (generate-file-name MEAN i) data)
    ))

(module+ mainc (main-c))

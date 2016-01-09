;;#! /usr/bin/env racket -tm
#lang racket

(provide (all-defined-out))
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt" plot)
(plot-new-window? #t)

;; CONFIGURATION

(define lab1-dir "/Users/linhchi.nguyen/Dropbox/fsm-bar/grand/deltas/c/run2/")
(define disa-lab "C:/Documents and Settings/linhchi.nguyen/My Documents/Dropbox/fsm-bar/grand/deltas/run2/")

;; change the directory of output file here
(define MEAN (string-append lab1-dir "mean"))
(define RANK (string-append lab1-dir "rank"))
(define PIC (string-append lab1-dir "meanplot"))

(define N 100)
(define P (build-random-population N))
(define CYCLES 500000)
(define SPEED 15)
(define ROUNDS-PER-MATCH 300)
(define DELTAS (list 0 .99 .97 .95 .93 .91 .9 .88 .86 .84 .82 .8 .78 .76 .74 .7))
(define MUTATION 1)

;; UTILITIES
(define (delta->string delta)
  (string-trim (number->string (* 100 delta)) ".0"))
(define (generate-file-name prefix delta)
  (string-append prefix (delta->string delta)))

;; MAIN
(define (evolve-d population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up-c population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-c p3 mutation)
         (define ranking-list (hash->list (rank p3)))
         (out-rank (generate-file-name RANK delta) cycles ranking-list)
         (cons (relative-average pp 1)
               (evolve-d p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(define (evolve-delta delta)
  (evolve-d P CYCLES SPEED ROUNDS-PER-MATCH delta MUTATION))

(define (main)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
	(define pic-name (configuration-string N SPEED ROUNDS-PER-MATCH i GAMMA))
    (define datas (time (evolve-delta i)))
    (plot (list (simulation->lines datas))
          #:width 1200
          #:y-min 0.0 #:y-max 10 #:title pic-name
          #:out-file (string-append (generate-file-name PIC i) ".png")
          )
    (out-mean (generate-file-name MEAN i) datas)
    ))

#! /usr/bin/env racket -tm
#lang racket

(provide (all-defined-out))

(require "automata.rkt" "population.rkt" "utilities.rkt" "scan.rkt" "in.rkt" plot)

(plot-new-window? #t)



(define (main)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define N 1000)
  (define CYCLES 1000)
  (define SPEED 100)
  (define ROUNDS-PER-MATCH 1)
  (define DELTA 1)
  (define MUTATION 1)
  (define datas
    (time (evolve (build-random-population N) CYCLES SPEED ROUNDS-PER-MATCH DELTA MUTATION)))
  (define ps (simulation->lines (map first datas)))
  (define ts (simulation->lines (map second datas)))
  (define rd (lines (map third datas)))
  (define h3 (function (lambda (x) 8) #:color "red"))
  (define h2 (function (lambda (x) 5) #:color "green"))
  (define h1 (function (lambda (x) 2) #:color "blue"))
  ; (out-mean ps)
  (plot (list h3 h2 h1 ps) #:y-min 0.0 #:y-max 10.0 #:title "mean")
  (plot (list ts) #:y-min 0.0 #:y-max N #:title "types#")
  (plot rd #:x-min 0.0 #:x-max N #:y-min 0 #:y-max N #:title "replicator dynamics"))

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
         ; (out-rank cycles p3 10 "rank")
         (cons (list (relative-average pp rounds-per-match)
                     (length (rank p3))
                     (scan-oneshot-types p3))
               (evolve p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(module+ five
  (main)
  (main)
  (main)
  (main)
  (main))

;; TEST 1: ONE SHOT REPLICATOR DYNAMICS

(define (build-oneshot-population l m h)
  (define p
    (vector-append
     (build-vector l (lambda (_) (lows 0)))
     (build-vector m (lambda (_) (mediums 0)))
     (build-vector h (lambda (_) (highs 0)))))
  (shuffle-vector p p))

(define point-list
  (list
   (list 900 50 50)
   (list 800 50 150)
   (list 50 50 900)
   (list 50 150 800)
   (list 800 150 50)
   (list 700 250 50)))

(define (evolve-rd population cycles speed r d)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population r d))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (cons (scan-oneshot-types p3)
               (evolve-rd p3 (- cycles 1) speed r d))]))

(define (test1 test-point file-name)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define A (apply build-oneshot-population test-point))
  (define N 1000)
  (define CYCLES 1000)
  (define SPEED 100)
  (define ROUNDS-PER-MATCH 1)
  (define DELTA 1)
  (define rd-types
    (time (evolve-rd A CYCLES SPEED ROUNDS-PER-MATCH DELTA)))
  (out-data file-name (map list (flatten rd-types)))
  (define rd (lines rd-types))
  (plot rd #:x-min 0.0 #:x-max N #:y-min 0 #:y-max N #:title "replicator dynamics"))

(define (test1s test-points)
  (for ([i (length test-points)])
    (test1 (list-ref test-points i)
           (string-append "rd" (number->string i)))))


;; TEST 2: REPEATED GAME RD FOR 4 TYPES

(define (build-test2-population m h a l)
  (define p
    (vector-append
     (build-vector l (lambda (_) (lows 0)))
     (build-vector m (lambda (_) (mediums 0)))
     (build-vector h (lambda (_) (highs 0)))
     (build-vector a (lambda (_) (accommodator 0)))))
  (shuffle-vector p p))

(define point-list2
  (list
   (list 900 50 49 1)
   (list 800 50 149 1)
   (list 50 50 899 1)
   (list 50 150 799 1)
   (list 800 150 49 1)
   (list 700 250 49 1)))

(define (evolve-rd2 population cycles speed r d)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population r d))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (cons (scan-4-types p3)
               (evolve-rd2 p3 (- cycles 1) speed r d))]))

(define (test2 test-point file-name)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define A (apply build-test2-population test-point))
  (define N 1000)
  (define CYCLES 1000)
  (define SPEED 100)
  (define ROUNDS-PER-MATCH 20)
  (define DELTA 1)
  (define rd-types
    (time (evolve-rd2 A CYCLES SPEED ROUNDS-PER-MATCH DELTA)))
  (out-data file-name (map list (flatten rd-types)))
  (define rd (lines rd-types))
  (plot rd #:x-min 0.0 #:x-max N #:y-min 0 #:y-max N #:title "replicator dynamics"))

(define (test2s test-points)
  (for ([i (length test-points)])
    (test2 (list-ref test-points i)
           (string-append "rd" (number->string i)))))

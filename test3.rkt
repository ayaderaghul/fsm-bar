#lang racket

(provide (all-defined-out))
(require "automata.rkt" "population.rkt"
         "utilities.rkt" "scan.rkt" "in.rkt"
         plot)
(plot-new-window? #t)

(define (build-random-test3 n)
  (define p
    (for/vector ([i (in-range n)])
      (define r (random 4))
      ((list-ref AUTO-SET r))))
  (cons p p))

;; output a readme that contains configuration

(define MEAN "/Users/linhchi.nguyen/Dropbox/fsm-bar/run5/mean")
(define RANK "/Users/linhchi.nguyen/Dropbox/fsm-bar/run5/rank")
(define PIC "/Users/linhchi.nguyen/Dropbox/fsm-bar/run5/mean.png")
(define RME "/Users/linhchi.nguyen/Dropbox/fsm-bar/run5/readme")

(define (configuration lst)
  (out-data RME
            (list lst)))

(define (run)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define N 1000)
  (define P (build-random-test3 N))
  (define CYCLES 200000)
  (define SPEED 100)
  (define ROUNDS-PER-MATCH 20)
  (define DELTA 1)
  (define MUTATION 1)
  (configuration (list N SPEED ROUNDS-PER-MATCH DELTA MUTATION))
  (define datas
    (time (evolve-t3 P CYCLES SPEED ROUNDS-PER-MATCH DELTA MUTATION)))
  (define ps (map first datas)) ; mean
  (define ts (map second datas)) ; number of types
  (define rs (map third datas)) ; highest ranking in each cycles
  (define mean-types# (/ (apply + ts) CYCLES))
  (define h3 (function (lambda (x) 8) #:color "red"))
  (define h2 (function (lambda (x) 5) #:color "green"))
  (define h1 (function (lambda (x) 2) #:color "blue"))
  (plot (list h3 h2 h1
              (simulation->lines ps)) #:y-min 0.0 #:y-max 10.0 #:title "mean" #:out-file PIC #:width 1000)
  (plot (list (simulation->lines ts)) #:y-min 0.0 #:y-max (+ 10 mean-types#) #:title "types#")
  (plot (list (simulation->lines rs)) #:y-min 0.0 #:y-max N #:title "biggest")
  (out-mean MEAN ps)
  )

(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

(define AUTO-SET
  (list mediums tough bully accommodator))

(define (mutate3 population0 mutation)
  (define p1 (car population0))
  (for ([i mutation])
    (define posn (random (vector-length p1)))
    (define r (random 4))
    (define mutated ((list-ref AUTO-SET r)))
    (vector-set! p1 posn mutated)))

(define (evolve-t3 population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate3 p3 mutation)
         (define ranking (rank p3))
         (define ranking-list (hash->list ranking))
         (out-rank RANK cycles ranking-list)
         (cons (list (relative-average pp rounds-per-match)
                     (hash-count ranking)
                     (apply max (if (hash-empty? ranking) (list 0) (hash-values ranking)))
                     )
               (evolve-t3 p3 (- cycles 1) speed rounds-per-match delta mutation))]))

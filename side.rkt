#lang racket

(provide main)
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt"
         plot)
(plot-new-window? #t)

;; TODO
;; this side projects needs to:
;; run simulation across deltas
;; and gammas
;; hm, normalise (1-delta) is when rounds = infinite

;; CONFIGURATION
;; change directory of output here

(define lab1-directory "/Users/linhchi.nguyen/Dropbox/fsm-bar/deltas/run2/")

(define MEAN "mean")
(define RANK "rank")
(define PIC "meanplot")

(define N 100)
(define CYCLES 100)
(define SPEED 15)
(define ROUNDS-PER-MATCH 15)
(define DELTAS (list 0 .3 .6 .7 .8 .9 .95 1))
(define MUTATION 1)
(define AUTO-SET (list mediums tough bully accommodator))

;; UTILITIES
;; the functions in this side project will have suffix -s for side or spinoff
(define (build-population-s n)
  (define p
    (for/vector ([i (in-range n)])
      (define r (random 4))
      ((list-ref AUTO-SET r))))
  (shuffle-vector p p))
(define P (build-population-s N))

(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))
(define (delta->string delta)
  (string-trim (number->string (* 100 delta)) ".0"))
(define (generate-file-name prefix delta)
  (string-append prefix (delta->string delta)))

(define (mutate-s population0 mutation)
  (define p1 (car population0))
  (for ([i mutation])
    (define posn (random (vector-length p1)))
    (define r (random 4))
    (define mutated ((list-ref AUTO-SET r)))
    (vector-set! p1 posn mutated)))

;; MAIN
(define (evolve-s population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-s p3 mutation)
         (define ranking (rank p3))
         (define ranking-list (hash->list ranking))
         (out-rank (generate-file-name RANK delta) cycles ranking-list)
         (cons (relative-average pp rounds-per-match)
               (evolve-s p3 (- cycles 1) speed rounds-per-match delta mutation))]))

(define (evolve-delta delta)
  (evolve-s P CYCLES SPEED ROUNDS-PER-MATCH delta MUTATION))

(define (main)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (configuration-string N SPEED ROUNDS-PER-MATCH i))
    (define datas
      (time (evolve-s P CYCLES SPEED ROUNDS-PER-MATCH i MUTATION)))
    (define max-pay (apply max datas))
    (plot (list (simulation->lines datas))
          #:y-min 0.0 #:y-max (+ 3 max-pay) #:title pic-name
          #:out-file (string-append (generate-file-name PIC i) ".png") #:width 1200)
    (out-mean (generate-file-name MEAN i) datas)
  ))

#lang racket

(provide main)
(require "automata.rkt" "population.rkt"
         "scan.rkt" "inout.rkt"
         plot)
(plot-new-window? #t)

;; CONFIGURATION
;; change directory of output here
<<<<<<< HEAD
(define MEAN "mean.txt")
(define RANK "rank.txt")
(define PIC "mean.png")

(define N 500)
(define CYCLES 100)
(define SPEED 200)
(define ROUNDS-PER-MATCH 15)
=======
(define MEAN "/Users/linhchi.nguyen/Dropbox/fsm-bar/run15/mean.txt")
(define RANK "/Users/linhchi.nguyen/Dropbox/fsm-bar/run15/rank.txt")
(define PIC "/Users/linhchi.nguyen/Dropbox/fsm-bar/run15/mean.png")
(define RME "/Users/linhchi.nguyen/Dropbox/fsm-bar/run15/readme.txt")

(define AUTO-SET
  (list mediums tough bully accommodator))
(define N 100)
(define CYCLES 70000)
(define SPEED 20)
(define ROUNDS-PER-MATCH 5)
>>>>>>> db9e3d36f86f3830ea32891d767ce84bc18b9e99
(define DELTA .95)
(define MUTATION 50)
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

(define (mutate-s population0 mutation)
  (define p1 (car population0))
  (for ([i mutation])
    (define posn (random (vector-length p1)))
    (define r (random 4))
    (define mutated ((list-ref AUTO-SET r)))
    (vector-set! p1 posn mutated)))

;; MAIN
(define (main)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-name (configuration-string N SPEED ROUNDS-PER-MATCH DELTA))
  (define datas
    (time (evolve-s P CYCLES SPEED ROUNDS-PER-MATCH DELTA MUTATION)))
  (define ps (map first datas)) ; mean
  (define ts (map second datas)) ; number of types
  (define rs (map third datas)) ; highest ranking in each cycles
  (define mean-types# (/ (apply + ts) CYCLES))
<<<<<<< HEAD
  (plot (list (simulation->lines ps)) #:y-min 0.0 #:y-max 5.0 #:title pic-name #:out-file PIC #:width 1000)
  (plot (list (simulation->lines ts)) #:y-min 0.0 #:y-max (+ 10 mean-types#) #:title "types#")
  (plot (list (simulation->lines rs)) #:y-min 0.0 #:y-max N #:title "biggest")
=======
  (plot (list (simulation->lines ps)) #:y-min 0.0 #:y-max 5.0 #:title "mean" #:out-file PIC #:width 1200)
  (plot (list (simulation->lines ts)) #:y-min 0.0 #:y-max (+ 10 mean-types#) #:title "types#" #:width 1200)
  (plot (list (simulation->lines rs)) #:y-min 0.0 #:y-max N #:title "biggest" #:width 1200)
>>>>>>> db9e3d36f86f3830ea32891d767ce84bc18b9e99
  (out-mean MEAN ps)
  )


(define (evolve-s population cycles speed rounds-per-match delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population rounds-per-match delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-s p3 mutation)
         (define ranking (rank p3))
         (define ranking-list (hash->list ranking))
         (out-rank RANK cycles ranking-list)
         (cons (list (relative-average pp rounds-per-match)
                     (hash-count ranking)
                     (apply max (if (hash-empty? ranking) (list 0) (hash-values ranking))))
               (evolve-s p3 (- cycles 1) speed rounds-per-match delta mutation))]))

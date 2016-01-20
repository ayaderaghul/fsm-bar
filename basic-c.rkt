#lang racket

(provide (all-defined-out))
(require "configuration.rkt" "automata.rkt" "population.rkt" "scan.rkt" "inout.rkt" plot/no-gui)



;; CONTINUAL PROBABILITY
(define (run-c)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-name (configuration-string DELTA "continual probability"))
  (define data
    (time (evolve-c (build-random-population N STATE#)
                    CYCLES SPEED ROUNDS DELTA PIE MUTATION)))
  (plot-payoff data DELTA 10.0 pic-name PIC)
  ;;(out-mean MEAN data)
  )

(module+ runc (run-c))

(define (evolve-c population cycles speed rounds-per-match delta pie mutation)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up-c population rounds-per-match delta pie))
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
                (evolve-c p3 (- cycles 1) speed rounds-per-match delta pie mutation))]))

(define (main-c)
  (for ([i (in-list DELTAS-C)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (configuration-string i "continual probability"))
    (define data (time (evolve-c (build-random-population N STATE#) CYCLES SPEED ROUNDS i PIE MUTATION)))
    (plot-payoff data 10.0 pic-name (generate-file-name PIC i))
    ;;(out-mean (generate-file-name MEAN i) data)
    ))

(module+ mainc (main-c))

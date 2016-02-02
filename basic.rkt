#lang racket

(provide evolve print-delta)
(require "utilities.rkt" "configuration.rkt" "./automata/automata.rkt" "./automata/personality.rkt" "population.rkt" "scan.rkt" "inout.rkt")

;; DISCOUNT FACTOR
(define (print-delta delta method)
  (print-configuration STATE# PIE method delta))

(define (run-d)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-title (print-delta DELTA "discount factor"))
  (define rank-name (generate-file-name DELTA RANK))
  (define mean-name (generate-file-name DELTA MEAN))
  ;(define res-name (generate-file-name DELTA RES))
  (define datas
    (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS DELTA PIE MUTATION mean-name rank-name)))
  (define ps (map first datas))
  (define ts (map second datas))
(define bs (map third datas))
(define fs (map fourth datas))
(define as (map fifth datas))
  ;(plot-distributions rs res-name)
  (plot-payoffs ps DELTA pic-title PIC)
(plot-payoff ts "toughs" "toughs.png")
(plot-payoff bs "bullys" "bullys.png")
(plot-payoff fs "fairs" "fairs.png")
(plot-payoff as "accoms" "accoms.png")
  )

(module+ main (run-d))

(define (evolve population cycles speed rounds-per-match delta pie mutation mean-file rank-file)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up-d population rounds-per-match delta pie))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-c p3 mutation)
         (define ranking (rank* p3))
         (define ranking-list (hash->list ranking))
(define autos (map car ranking-list))
(define auto-numbers (map cdr ranking-list))
(define char-hash 
(if (empty? ranking-list) (hash 'nothing (list 0 0))
(hash
     (first (test-mixture autos auto-numbers rounds-per-match delta pie))
     (list cycles (apply + auto-numbers)))))
         (when (zero? (modulo cycles DATA-POINT))
              (out-rank rank-file cycles
                       (hash->list (rank p3))))
         (define m (relative-average pp 1))
         (out-mean mean-file (list m))
         (cons (list m char-hash)
               ;;(hash-count ranking)
               ;;(apply max (if (hash-empty? ranking) (list 0) (hash-values ranking))))
               (evolve p3 (- cycles 1) speed rounds-per-match delta pie mutation mean-file rank-file))]))

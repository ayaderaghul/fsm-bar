#lang racket

(provide evolve print-delta)
(require "utilities.rkt" "configuration.rkt" "automata.rkt" "population.rkt" "scan.rkt" "inout.rkt")

;; DISCOUNT FACTOR
(define (print-delta delta method)
  (print-configuration STATE# PIE method delta))

(define (run-d)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-title (print-delta DELTA "discount factor"))
  (define rank-name (generate-file-name RANK DELTA))
  (define mean-name (generate-file-name MEAN DELTA))
  (define res0-name (generate-file-name RES0 DELTA))
  (define res5-name (generate-file-name RES5 DELTA))
  (define datas
    (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS DELTA PIE MUTATION mean-name rank-name)))
  (define ps (map first datas))
  ;(define r0 (map second datas))
  ;(define r5 (map third datas))
  ;(plot-xxx r0 res0-name)
  ;(plot-xxx r5 res5-name)
  (plot-payoffs ps DELTA pic-title PIC)
  ;;(plot-as as0 (+ 10 max-as0) "responses to h - 0th order" res0-name)
  ;;(plot-as as5 (+ 10 max-as5) "responses to h - 5th order" res5-name)
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
         (define sample-auto
           (if (hash-empty? ranking) 0 (car (first ranking-list))))
         (define resp0
           (if (struct? sample-auto)
               (core-responses-3 0 sample-auto)
               (list (list 0 0 0) (list 0 0 0) (list 0 0 0))))
         (define resp5
           (if (struct? sample-auto)
               (core-responses-3 5 sample-auto)
               (list (list 0 0 0) (list 0 0 0) (list 0 0 0))))
(when (zero? (modulo cycles DATA-POINT))
              (out-rank rank-file cycles
                       (hash->list (rank p3))))
         (define m (relative-average pp 1))
         (out-mean mean-file (list m resp0 resp5))
         (cons (list m resp0 resp5)
               ;;(hash-count ranking)
               ;;(apply max (if (hash-empty? ranking) (list 0) (hash-values ranking))))
               (evolve p3 (- cycles 1) speed rounds-per-match delta pie mutation mean-file rank-file))]))


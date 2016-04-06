#lang racket

(provide evolve print-delta)
(require
 "./automata/personality.rkt"
"utilities.rkt"
"configuration.rkt"
"population.rkt"
"scan.rkt"
"inout.rkt"
"draw.rkt")

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
  (define test-name (generate-file-name DELTA TESTS))
  (define datas
    (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS DELTA PIE MUTATION mean-name rank-name)))
  ;(define ps (map first datas))
  ;(define char-hash (map second datas))
;(define char-hash-m (map third datas))
;(define char-list (render-characters (flatten char-hash)))
;(define char-list-m (render-characters char-hash-m))
(draw-bundle-f mean-name DELTA rank-name make-automaton-csv-reader DATA-POINT ROUNDS PIE (string-append test-name ".png"))
  )

(module+ main (run-d))

(define (evolve population cycles speed rounds-per-match delta pie mutation mean-file rank-file)
  (define (out-data* lst) (flatten lst))
  (define out-test1 (string-append rank-file "1"))
  (define out-test2 (string-append rank-file "2"))
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up-d population rounds-per-match delta pie))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-c p3 mutation)
         (define ranking (rank* p3))
         (define ranking-list (hash->list ranking))
         (match-define
          (list char-result1 char-result2)
          (test-simulation (list ranking-list) DATA-POINT rounds-per-match delta pie))
         (match-define
          (list result1 result2)
          (list
           (map hash->list char-result1)
           (map hash->list char-result2)))
         (match-define
          (list out1 out2)
          (list
           (map out-data* result1)
           (map out-data* result2)))
         (out-data out-test1 out1)
         (out-data out-test2 out2)
         (when (zero? (modulo cycles DATA-POINT))
           (out-rank rank-file cycles
                     (hash->list (rank p3))))
         (define m (relative-average pp 1))
         (out-mean mean-file (list m))
         (cons m ;char-results
               ;;(hash-count ranking)
               ;;(apply max (if (hash-empty? ranking) (list 0) (hash-values ranking))))
               (evolve p3 (- cycles 1) speed rounds-per-match delta pie mutation mean-file rank-file))]))

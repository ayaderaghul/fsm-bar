#lang racket

(provide (all-defined-out))
(require "configuration.rkt" "automata.rkt" "population.rkt" "scan.rkt" "inout.rkt" plot/no-gui)

;; UTILITIES
(define (configuration-string delta method)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, pie = ~a, ~a: delta = ~a"
   N SPEED ROUNDS STATE# PIE method delta))

(define (pack-coors data)
  (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
(define (plot-payoff lst y-max title file-name) ;; for the continual method (no ceiling)
  (plot-file (simulation->lines lst) file-name 'png
        #:y-min 0.0 #:y-max y-max #:title title
                #:width 1200))

(define (plot-x lst title file-name)
  (define x->l (pack-coors (map first lst)))
  (define x->m (pack-coors (map second lst)))
  (define x->h (pack-coors (map third lst)))
  (define y-max (apply max (flatten lst)))
  (plot-file (list
              (lines x->l #:color 'brown)
              (lines x->m #:color 'green)
              (lines x->h #:color 'red)) file-name 'png
             #:y-min 0.0 #:y-max (+ 10 y-max) #:title title
             #:width 1200))

(define (plot-xxx lst pre-name)
(define l (map first lst))
(define m (map second lst))
(define h (map third lst))
(plot-x l "l->" (string-append pre-name "l"))
(plot-x m "m->" (string-append pre-name "m"))
(plot-x h "h->" (string-append pre-name "h"))
)

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
(define (main)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define pic-name (configuration-string DELTA "discount factor"))
  (define rank-name (generate-file-name RANK DELTA))
  (define mean-name (generate-file-name MEAN DELTA))
  (define res0-name (generate-file-name RES0 DELTA))
  (define res5-name (generate-file-name RES5 DELTA))
  (define datas
    (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS DELTA PIE MUTATION mean-name rank-name)))
  (out-mean MEAN datas)
  (define ps (map first datas))
  (define r0 (map second datas))
  (define r5 (map third datas))
  (plot-xxx r0 res0-name)
  (plot-xxx r5 res5-name)
  (plot-payoffs ps DELTA pic-name PIC)
  ;;(plot-as as0 (+ 10 max-as0) "responses to h - 0th order" res0-name)
  ;;(plot-as as5 (+ 10 max-as5) "responses to h - 5th order" res5-name)
  ;;(plot (simulation->lines as) #:width 1200)
  ;;
  )

(define (evolve population cycles speed rounds-per-match delta pie mutation mean-file rank-file)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up-d population rounds-per-match delta pie))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (mutate-c p3 mutation)
         ;;(when (zero? (modulo cycles DATA-POINT))
           ;;(begin
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
             ;;(out-rank rank-file cycles
             ;;          (hash->list (rank p3))))
         (define m (relative-average pp 1))
             ;;(out-mean mean-file (list m))
         (cons (list m resp0 resp5)
               ;;(hash-count ranking)
               ;;(apply max (if (hash-empty? ranking) (list 0) (hash-values ranking))))
               (evolve p3 (- cycles 1) speed rounds-per-match delta pie mutation mean-file rank-file))]))

`(module+ rund
  (run))

`(define (main)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (configuration-string i "discount factor"))
(define rank-name (generate-file-name RANK i))
    (define data (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS i PIE MUTATION rank-name)))
    (plot-payoffs data i pic-name (generate-file-name PIC i))
    (out-mean (generate-file-name MEAN i) data)
    ))

;; ACROSS PIE
(define (print-configuration pie)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, pie = ~a, ~a: delta = ~a"
   N SPEED ROUNDS STATE# pie "discount factor" DELTA))
(define (pies)
  (for ([i (in-list PIES)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (print-configuration i))
(define rank-name (generate-file-name RANK i))
    (define data (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS DELTA i MUTATION rank-name)))
    (plot-payoffs data DELTA pic-name (generate-file-name PIC i))
    (out-mean (generate-file-name MEAN i) data)
    ))
(module+ pies (pies))


;; ACROSS STATES#
(define (print-title state#)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, pie = ~a, ~a: delta = ~a"
   N SPEED ROUNDS state# PIE "discount factor" DELTA))
(define (states#)
  (for ([i (in-list STATES#)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-name (print-title i))
(define rank-name (generate-file-name RANK i))
    (define data (time (evolve (build-random-population N i) CYCLES SPEED ROUNDS DELTA PIE MUTATION rank-name)))
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

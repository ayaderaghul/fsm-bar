#lang racket

(require "basic.rkt" "configuration.rkt" "utilities.rkt" "population.rkt")

;; ACROSS DELTAS: DISCOUNT FACTOR

(define (deltas)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-title (print-delta i "discount factor"))
    (define pic-name (generate-file-name i PIC))    
    (define mean-name (generate-file-name i MEAN))
    (define rank-name (generate-file-name i RANK))
    ;(define res-name (generate-file-name i RES)) 
(define test-name (generate-file-name i TESTS))    
    (define datas (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS i PIE MUTATION mean-name rank-name)))
    (define ps (map first datas))
    (define char-hash (map second datas))
(define char-hash-m (map third datas))
(define char-list (render-characters (flatten char-hash)))
(define char-list-m (render-characters char-hash-m))
(draw-bundle ps char-list char-list-m DELTA (string-append test-name ".png"))
    ))

(module+ main (deltas))

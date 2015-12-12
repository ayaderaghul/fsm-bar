#lang racket
(provide (all-defined-out))

(define (monte expression)
  (time
   (for ([i 5000])
     expression)))

(define (monte-carlo expression)
  (for ([i 20])
    (monte expression)))

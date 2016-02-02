#lang racket

(require "inout.rkt" "configuration.rkt")
(provide main)
(define cluster "/home/linhchi.nguyen/run86/")


(define (main)
(plot-simulations 
(list (string-append cluster "rank0")
(string-append cluster "rank30")
(string-append cluster "rank70")
(string-append cluster "rank85")
(string-append cluster "rank90")
(string-append cluster "rank95")
(string-append cluster "rank99")
)
make-automaton-csv-reader
100
ROUNDS
(list 0 .3 .7 .85 .9 .95 .99)
PIE)
)

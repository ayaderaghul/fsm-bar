#lang racket
(provide (all-defined-out))
(require (planet neil/csv:2:0))
(define make-automaton-csv-reader
  (make-csv-reader-maker
   '((separator-chars #\,)
     (strip-leading-whitespace? . #t)
     (strip-trailing-whitespace? . #t))))


(define next-row
  (make-automaton-csv-reader (open-input-file "rank100aa")))

(define (all-rows)
  (define row (next-row))
  (cond [(empty? row) '()]
        [else
         (cons row (all-rows))]))

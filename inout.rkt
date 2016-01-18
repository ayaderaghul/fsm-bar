#lang racket
(provide (all-defined-out))
(require "automata.rkt" "csv.rkt" 2htdp/batch-io plot/no-gui "configuration.rkt")
(require (planet neil/csv:2:0))

;; IMPORT

(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

;;
(define (load-strings csv-file)
  (csv->list (open-input-file csv-file)))

(define (load-data csv-file)
  (define a (load-strings csv-file))
  (define b (map first a))
  (map string->number b))
;;
(define make-automaton-csv-reader
  (make-csv-reader-maker
   '((separator-chars #\,)
     (strip-leading-whitespace? . #t)
     (strip-trailing-whitespace? . #t))))
(define (all-rows file make-reader)
  (define next-row
    (make-reader (open-input-file file)))
  (define (loop)
    (define row (next-row))
    (if (empty? row) '()
        (cons row (loop))))
  (loop))

(define (at-row n file make-reader)
  (define next-row (make-reader (open-input-file file)))
  (define (at x)
    (define row (next-row))
    (if (zero? x) row (at (- x 1))))
  (at n))

(define (plot-mean csv-file title pic-name)
  (define c (load-data csv-file))
  (define d (simulation->lines c))
  (define max-pay (* 5 ROUNDS DELTA))
  (define ceiling (function (lambda (x) max-pay) #:color "blue"))
  (plot-file (list d ceiling) pic-name 'png #:width 1200 #:y-max (+ 10 max-pay) #:title title))

;; load dynamics
(define (pack-coors a-list)
  [define l (length a-list)]
  (for/list ([i (in-range (/ l 2))])
    (list
     (list-ref a-list (* 2 i))
     (list-ref a-list (add1 (* 2 i))))))
(define (load-dynamic csv-file)
  (lines (pack-coors (load-data csv-file))))
(define (load-dynamics file-list)
  [define l (length file-list)]
  (for/list ([i (in-range l)])
    (load-dynamic (list-ref file-list i))))

;; EXPORT DATA
;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

(define (out-mean filename data)
  (out-data filename (map list data)))

(define (out-rank filename day data)
  (out-data filename (append (list (list day)
                                 (map list data)))))

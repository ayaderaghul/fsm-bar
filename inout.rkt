#lang racket
(provide (all-defined-out))
(require "automata.rkt" "csv.rkt" 2htdp/batch-io plot)

;; IMPORT

(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

(define (load-data* csv-file)
  (define a (read-csv-file csv-file))
  (define b (map first a))
  (map string->number b))

(define (plot-mean csv-file title pic-name)
  (define c (load-data* csv-file))
  (define d (simulation->lines c))
  (define max-pay (apply max c))
  (plot d #:width 1200 #:y-max (+ 1 max-pay) #:title title #:out-file pic-name))   

(define (load-data csv-file)
  [define strings (read-csv-file csv-file)]
  [define l (length strings)]
  [define-values (data)
    (for/fold ([data '()])
              ([i (in-range l)])
      [define datum (apply string->number (list-ref strings i))]
      (values (cons datum data)))]
  (reverse data))

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

(define (configuration-string N speed rounds delta gamma)
  (format
   "N = ~a, speed = ~a, rounds = ~a, delta = ~a, gamma = ~a"
   N speed rounds delta gamma))

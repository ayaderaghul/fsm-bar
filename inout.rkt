#lang racket
(provide (all-defined-out))
(require "utilities.rkt" "automata.rkt" "csv.rkt" plot/no-gui "configuration.rkt")
(require (planet neil/csv:2:0))

;; IMPORT

;; PLOT MEANS FROM EXPORTED DATA

(define (load-string csv-file)
  (csv->list (open-input-file csv-file)))
(define (load-data csv-file)
  (define a (load-string csv-file))
  (define b (map first a))
  (map string->number b))

(define (plot-mean csv-file title pic-name)
  (define c (load-data csv-file))
  (define d (series->lines c))
  (define max-pay (* 5 (compound DELTA ROUNDS)))
  (define ceiling (function (lambda (x) max-pay) #:color "blue"))
  (plot-file (list d ceiling) pic-name 'png #:width 1200 #:y-max (+ 10 max-pay) #:title title))

;; PLOT DYNAMICS FROM EXPORTED DATA

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


;; IMPORT AUTOMATA
;; converting automaton back from data is very slow
;; let's calculate what you need in the process then
;; export only numeric data
#|
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
(define (posn->cycle p)
  (* DATA-POINT (/ p 2)))
(define (take-odd lst)
  (define l (length lst))
  (filter-not false?
              (for/list ([i (in-range l)]
                         [j (in-list lst)])
                (and (odd? i) j))))
(define (converts lst)
  (for/list ([i (in-range (length lst))]
             [j (in-list lst)])
    (cond [(and (zero? (- (length j) 1))
                (zero? (apply string-length j)))
           0]
          [else (map convert j)])))
(define (converts2 lst)
  (for/fold ([results '()])
            ([i (in-range (length lst))]
             [j (in-list lst)])
    (define result
      (cond [(and (zero? (- (length j) 1))
                  (zero? (apply string-length j)))
             0]
            [else (map convert j)]))
    (cons result results)))
(define (convert x)
  (define y (string-split x " . "))
  (define (trim-bracket a)
    (define b (string-trim a "(" #:repeat? #t))
    (string-trim b ")" #:repeat? #t))
  (match-define (list z t) (map trim-bracket y))
  (cons
   (recover (map string->number (string-split z)))
   (string->number t)))
|#


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

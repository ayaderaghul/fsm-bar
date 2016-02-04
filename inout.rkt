#lang racket
(provide (all-defined-out))
(require "./automata/automata.rkt" "./automata/personality.rkt"
         "csv.rkt" plot/no-gui
         "configuration.rkt" "utilities.rkt")
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
  (define cap (function (lambda (x) max-pay) #:color "blue"))
  (plot-file (list d cap) pic-name 'png #:width 1200 #:y-min 0.0 #:y-max (+ 10 max-pay) #:title title))

(define (load-mean csv-file)
(define c (load-data csv-file))
(series->lines c))

;; PLOT DYNAMICS FROM EXPORTED DATA

(define (load-dynamic csv-file)
  (lines (pack-coors (load-data csv-file))))
(define (load-dynamics file-list)
  [define l (length file-list)]
  (for/list ([i (in-range l)])
    (load-dynamic (list-ref file-list i))))


;; IMPORT AUTOMATA

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
(define (cycle->posn c data-point)
  (* 2 (/ c data-point)))


;; convert the whole file at once, this shouldnt be necessary

(define (take-odd lst)
  (define l (length lst))
  (filter-not false?
              (for/list ([i (in-range l)]
                         [j (in-list lst)])
                (and (odd? i) j))))
(define (take-even lst)
  (define l (length lst))
  (filter-not false?
              (for/list ([i (in-range l)]
                         [j (in-list lst)])
                (and (even? i) j))))
(define (converts lst)
  (for/list ([i (in-range (length lst))]
             [j (in-list lst)])
    (cond [(and (zero? (- (length j) 1))
                (zero? (apply string-length j)))
           '()]
          [else (map convert j)])))

(define (converts2 lst)
  (reverse
   (for/fold ([results '()])
             ([i (in-range (length lst))]
              [j (in-list lst)])
     (define result
       (cond [(and (zero? (- (length j) 1))
                   (zero? (apply string-length j)))
              0]
            [else (map convert j)]))
     (cons result results))))
(define (converts3 lst)
  (define result
    (foldl (lambda (next init)
             (cons
              (cond [(and (zero? (- (length next) 1))
                          (zero? (apply string-length next)))
                     0]
                    [else (map convert next)])
              init))
           '()
           lst))
  (reverse result))

(define (convert x)
  (define y (string-split x " . "))
  (define (trim-bracket a)
    (define b (string-trim a "(" #:repeat? #t))
    (string-trim b ")" #:repeat? #t))
  (match-define (list z t) (map trim-bracket y))
  (cons
   (recover (map string->number (string-split z)))
   (string->number t)))

(define (resurrect pre lst)
  (define resurrected (map convert lst))
  (define l (length resurrected))
  (define names (generate-ax pre (build-list l values)))
  (for ([i (in-list names)]
        [j (in-list resurrected)])
    (eval (list 'define i (car j))))
  (map cdr resurrected))

(define (resurrect-at cycle data-point pre file make-reader)
  (define posn (cycle->posn cycle data-point))
  (define data (at-row (+ posn 1) file make-reader))
  (resurrect pre data))

;; name the resurrected automata
(define (x->ax pre x)
  (string->symbol (string-append pre (number->string x))))
(define (generate-ax pre a-list)
  (map (lambda (x) (x->ax pre x)) a-list))


;; test the whole simulation, then plot the characters
(define (render-characters-f file make-reader data-point
                                     rounds delta pie)
  (define data (all-rows file make-reader))
  (define autos (take-odd data))
  (define ressurected (converts autos))
  (define test-result (test-simulation ressurected data-point rounds delta pie))
  (map render-characters test-result))


(define (plot-simulation file make-reader data-point rounds delta pie)
  (define types-list (render-characters-f file make-reader data-point rounds delta pie))
  (plot-point-types (string-append (string-replace file "rank" "meanplot") "chars.png") types-list CHAR-LIST DARK-COLORS))

(define (plot-simulations file-list make-reader data-point rounds delta-list pie)
  (for ([i (in-list file-list)]
        [j (in-list delta-list)])
    (time (plot-simulation i make-reader data-point rounds j pie))))




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

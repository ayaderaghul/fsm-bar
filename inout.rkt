#lang racket
(provide (all-defined-out))
(require "utilities.rkt" "./automata/automata.rkt" "./automata/interaction.rkt" "./automata/personality.rkt" "csv.rkt" plot/no-gui "configuration.rkt" unstable/hash "scan.rkt")
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
  (plot-file (list d ceiling) pic-name 'png #:width 1200 #:y-min 0.0 #:y-max (+ 10 max-pay) #:title title))

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
(define (converts lst)
  (for/list ([i (in-range (length lst))]
             [j (in-list lst)])
    (cond [(and (zero? (- (length j) 1))
                (zero? (apply string-length j)))
           0]
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

;; match the resurrected automata
(define (contest lst rounds delta pie)
  (for/list ([i (in-list lst)])
    (for/list ([j (in-list lst)])
      (interact i j rounds delta pie))))

;; test the whole simulation, then plot the characters
(define (render-characters file make-reader data-point rounds delta pie)
  (define data (all-rows file make-reader))
  (define autos (take-odd data))
  (define ressurected (converts autos))
  (define test-result (test-simulation ressurected data-point rounds delta pie))
  (define len (length test-result))
  (define types (apply hash-union test-result
                       #:combine/key (lambda (k v1 v2) (append  v1 v2))))
  (define toughs (how-many 'tough types))
  (define bullys (how-many 'bully types))
  (define b-toughs (how-many 'bullyish-tough types))
  (define fairs (how-many 'fair types))
  (define accoms (how-many 'accommodator types))
(define a-accoms (how-many 'almost-accommodator types))
  (list toughs b-toughs bullys fairs accoms a-accoms))

(define (plot-types filename a-list name-list)
  (define len (length a-list))
  (define data
    (for/list ([i (in-list a-list)]
               [j (in-naturals len)]
               [k (in-list name-list)])
      (if (list? i)
          (lines (pack-coors i) #:color j #:label k)
          (lines (list (list 0 0)) #:color j #:label k))))
  (plot-file data filename 'png #:y-max 130 #:y-min 0 #:width 1200))

(define (plot-point-types filename a-list name-list color-list)
  (define data
    (for/list ([i (in-list a-list)]
               [j (in-list color-list)]
               [k (in-list name-list)]
               )
      (if (list? i)
          (points (pack-coors i) #:color j #:line-width 10 #:alpha .7 #:label k)
          (points (list (list 0 0)) #:color j #:line-width 10 #:label k))))
  (plot-file data filename 'png #:y-max 130 #:y-min 0 #:width 1200))

(define (plot-simulation file make-reader data-point rounds delta pie)
  (define types-list (render-characters file make-reader data-point rounds delta pie))
  (define name-list (list "toughs" "bullyish-toughs" "bullys" "fairs" "accommodators" "almost-accommodators"))
(define high-color-list (list 'darkblue 'royalblue  'cyan 'lime 'purple 'magenta))
(define dark-color-list (list 'midnightblue 'royalblue 'darkturquoise 'darkgreen 'crimson 'orchid))
  (plot-point-types (string-append file "chars.png") types-list name-list dark-color-list))

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

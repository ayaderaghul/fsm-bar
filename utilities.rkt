#lang racket
(require "configuration.rkt" plot/no-gui)
(provide (all-defined-out))

;; GENERATE NAMES AND TITLES
(define (print-configuration state# pie method delta)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, pie = ~a, ~a: delta = ~a"
   N SPEED ROUNDS state# pie method delta))

(define (variable->string x)
  (string-trim (number->string (* 100 x)) ".0"))
(define (generate-file-name prefix x)
  (string-append prefix (variable->string x)))

;; PLOT MEANS IN RUNTIME

(define (pack-points data)
  (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))

(define (series->lines data)
  (define points (pack-points data))
  (lines points))

(define (plot-payoff lst y-max title file-name) ;; for the continual method (no ceiling)
  (plot-file (series->lines lst) file-name 'png
        #:y-min 0.0 #:y-max y-max #:title title
                #:width 1200))

;; to calculate the compound rate of payoff
(define (compound d r) (foldl (lambda (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))

(define (plot-payoffs lst delta title file-name) ;; payoff series w ceiling - discount method
(define m-pay (* 5 (compound delta ROUNDS)))
(define ceiling (function (lambda (x) m-pay) #:color "blue"))
  (plot-file (list (series->lines lst) ceiling) (string-append file-name ".png") 'png
        #:y-min 0.0 #:y-max (+ 10 m-pay) #:title title
                #:width 1200))

;; PLOT RESPONSE DISTRIBUTION OF AUTOMATA
(define (plot-distribution lst title file-name)
  (define x->l (pack-points (map first lst)))
  (define x->m (pack-points (map second lst)))
  (define x->h (pack-points (map third lst)))
  (define y-max (apply max (flatten lst)))
  (plot-file (list
              (lines x->l #:color 'brown)
              (lines x->m #:color 'green)
              (lines x->h #:color 'red)) file-name 'png
             #:y-min 0.0 #:y-max (+ 10 y-max) #:title title
             #:width 1200))

(define (plot-distributions lst pre-name)
(define l (map first lst))
(define m (map second lst))
(define h (map third lst))
(plot-distribution l "l->" (string-append pre-name "l"))
(plot-distribution m "m->" (string-append pre-name "m"))
(plot-distribution h "h->" (string-append pre-name "h"))
)




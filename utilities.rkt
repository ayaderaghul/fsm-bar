#lang racket
(require "configuration.rkt" plot/no-gui unstable/hash)
(provide (all-defined-out))

;; GENERATE NAMES AND TITLES
(define (print-configuration state# pie method delta)
  (format
   "N = ~a, speed = ~a, rounds = ~a, states# = ~a, pie = ~a, ~a: delta = ~a, INV = ~a, CUS = ~a"
   N SPEED ROUNDS state# pie method delta INV CUS))

(define (variable->string x)
  (string-trim (number->string (* 100 x)) ".0"))
(define (generate-file-name x suffix)
  (string-append (variable->string x) suffix))

;; PLOT MEANS IN RUNTIME

(define (pack-coors a-list)
  [define l (length a-list)]
  (for/list ([i (in-range (/ l 2))])
    (list
     (list-ref a-list (* 2 i))
     (list-ref a-list (add1 (* 2 i))))))

(define (pack-points data)
  (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))

(define (series->lines data)
  (define points (pack-points data))
  (lines points))

(define (plot-payoff lst title file-name) ;; for the continual method (no ceiling)
(define max-pay (apply max lst))  
(plot-file (series->lines lst) (string-append file-name ".png") 'png
        #:y-min 0.0 #:y-max (+ 5 max-pay) #:title title
                #:width 1200))


;; to calculate the compound rate of payoff
(define (compound d r) (foldl (lambda (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))

(define (plot-payoffs lst delta title file-name) ;; payoff series w ceiling - discount method
;(define m-pay (* 5 (compound delta ROUNDS)))
(define m-pay 5) ; bc we normalised popu mean by dividing by (compound d r)
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

;; PLOT CHARACTERS IN RUNTIME


(define (how-many x h)
(if (hash-has-key? h x) (hash-ref h x) (hash-ref h 'nothing)))

(define (render-characters hash-lst)
(define types (apply hash-union hash-lst
               #:combine/key (lambda (k v1 v2) (append  v1 v2))))
(define toughs (how-many 'tough types))
  (define bullys (how-many 'bully types))
  (define b-toughs (how-many 'bullyish-tough types))
  (define fairs (how-many 'fair types))
  (define accoms (how-many 'accommodator types))
  (define a-accoms (how-many 'almost-accommodator types))
(define highs (how-many 'high types))
(define lows (how-many 'low types))
  (list toughs b-toughs bullys fairs accoms a-accoms highs lows))


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

(define (plot-point-types filename a-list alpha y-max)
  (define data
    (for/list ([i (in-list a-list)]
               [j (in-list DARK-COLORS)]
               [k (in-list CHAR-LIST)]
               )
          (points (pack-coors i) #:color j #:line-width 7 #:alpha alpha #:label k)))
         
  (plot-file data filename 'png #:y-max y-max #:y-min 0 #:width 1200))


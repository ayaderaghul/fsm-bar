#lang racket

(require "basic.rkt" "configuration.rkt" "utilities.rkt" "population.rkt" "inout.rkt" "scan.rkt" unstable/hash)

;; ACROSS DELTAS: DISCOUNT FACTOR

(define (deltas)
  (for ([i (in-list DELTAS)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define pic-title (print-delta i "discount factor"))
    (define pic-name (generate-file-name i PIC))    
    (define mean-name (generate-file-name i MEAN))
    (define rank-name (generate-file-name i RANK))
    ;(define res-name (generate-file-name i RES)) 
(define char-name (generate-file-name i CHAR))    
    (define datas (time (evolve (build-random-population N STATE#) CYCLES SPEED ROUNDS i PIE MUTATION mean-name rank-name)))
    (define ps (map first datas))
    (plot-payoffs ps i pic-title pic-name)
    (define chars (map second datas))
(define types (apply hash-union chars
                       #:combine/key (lambda (k v1 v2) (append  v1 v2))))

(define toughs (how-many 'tough types))
  (define bullys (how-many 'bully types))
  (define b-toughs (how-many 'bullyish-tough types))
  (define fairs (how-many 'fair types))
  (define accoms (how-many 'accommodator types))
(define a-accoms (how-many 'almost-accommodator types))
  (define type-list (list toughs b-toughs bullys fairs accoms a-accoms))
(define name-list (list "toughs" "bullyish-toughs" "bullys" "fairs" "accommodators" "almost-accommodators"))
(define dark-color-list (list 'midnightblue 'royalblue 'darkturquoise 'darkgreen 'crimson 'orchid))
(plot-point-types (string-append char-name ".png") type-list name-list dark-color-list)
    ))

(module+ main (deltas))

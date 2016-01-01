#lang racket

(require "automata.rkt" "population.rkt" "scan.rkt" "inout.rkt" plot)
(plot-new-window? #t)

(provide test1s test2s test3s plot-dynamics)

;; UTILITIES
  (define N 1000)
  (define CYCLES 1000)
  (define SPEED 100)
  (define ROUNDS-PER-MATCH 1)
  (define DELTA 1)

;; TEST 1: ONE SHOT REPLICATOR DYNAMICS
(define (build-oneshot-population l m h)
  (define p
    (vector-append
     (build-vector l (lambda (_) (lows 0)))
     (build-vector m (lambda (_) (mediums 0)))
     (build-vector h (lambda (_) (highs 0)))))
  (shuffle-vector p p))

(define point-list
  (list
   (list 900 50 50)
   (list 800 50 150)
   (list 50 50 900)
   (list 50 150 800)
   (list 800 150 50)
   (list 700 250 50)))

(define (evolve-rd population cycles speed r d)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population r d))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (cons (scan-oneshot-types p3)
               (evolve-rd p3 (- cycles 1) speed r d))]))

(define (test1 test-point file-name)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define A (apply build-oneshot-population test-point))
  (define rd-types
    (time (evolve-rd A CYCLES SPEED ROUNDS-PER-MATCH DELTA)))
  (out-data file-name (map list (flatten rd-types)))
  (define rd (lines rd-types))
  (plot rd #:x-min 0.0 #:x-max N #:y-min 0 #:y-max N #:title "replicator dynamics"))

(define (test1s test-points)
  (for ([i (length test-points)])
    (test1 (list-ref test-points i)
           (string-append "rd" (number->string i)))))


;; TEST 2: REPEATED GAME RD FOR 4 TYPES: LOWS MEDIUMS HIGHS ACCOMMODATOR
(define (build-test2-population m h a l)
  (define p
    (vector-append
     (build-vector l (lambda (_) (lows 0)))
     (build-vector m (lambda (_) (mediums 0)))
     (build-vector h (lambda (_) (highs 0)))
     (build-vector a (lambda (_) (accommodator 0)))))
  (shuffle-vector p p))

(define point-list2
  (list
   (list 250 700 49 1)
   (list 50 850 49 1)
   (list 900 50 49 1)
   (list 800 50 149 1)
   (list 50 50 899 1)
   (list 50 150 799 1)
   (list 800 150 49 1)
   (list 700 250 49 1)))

(define (evolve-rd2 population cycles speed r d)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population r d))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (cons (scan-mediums-highs p3)
               (evolve-rd2 p3 (- cycles 1) speed r d))]))

(define (test2 test-point file-name)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define A (apply build-test2-population test-point))
  (define rd-types
    (time (evolve-rd2 A CYCLES SPEED ROUNDS-PER-MATCH DELTA)))
  (out-data file-name (map list (flatten rd-types)))
  (define rd (lines rd-types))
  (plot rd #:x-min 0.0 #:x-max N #:y-min 0 #:y-max N #:title "replicator dynamics"))

(define (test2s test-points)
  (for ([i (length test-points)])
    (test2 (list-ref test-points i)
           (string-append "rd" (number->string i)))))

;; TEST 3: REPEATED GAME RD FOR 4 TYPES: MEDIUMS TOUGH BULLY ACCOMMODATOR
;; AND DELTA CHANGES
(define (build-test3-population f t b a)
  (define p
    (vector-append
     (build-vector f (lambda (_) (mediums)))
     (build-vector t (lambda (_) (tough)))
     (build-vector b (lambda (_) (bully)))
     (build-vector a (lambda (_) (accommodator)))))
  (shuffle-vector p p))

(define point-list3
  (list
   (list 150 750 50 50)
   (list 200 700 50 50)
   (list 250 650 50 50)
   (list 250 550 100 100)
   (list 250 450 150 150)
   (list 350 550 50 50)
   (list 150 250 300 300)
   (list 250 250 250 250)
   (list 350 250 200 200)
   ))

(define (evolve-rd3 population cycles speed r d)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-up* population r d))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (cons (scan-mediums-tough p3)
               (evolve-rd3 p3 (- cycles 1) speed r d))]))

(define (test3 test-point file-name)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define A (apply build-test3-population test-point))
  (define DELTAs .95)
  (define rd-types
    (time (evolve-rd3 A CYCLES SPEED ROUNDS-PER-MATCH DELTAs)))
  (out-data file-name (map list (flatten rd-types)))
  (define rd (lines rd-types))
  (plot rd #:x-min 0.0 #:x-max N #:y-min 0 #:y-max N #:title "replicator dynamics"))

(define (test3s test-points)
  (for ([i (length test-points)])
    (test3 (list-ref test-points i)
           (string-append "rd" (number->string i)))))

(define filelist3
  (list "rd0" "rd1" "rd2" "rd3" "rd4" "rd5" "rd6" "rd7" "rd8"))

(define (plot-dynamics file-list)
  (define data (load-dynamics file-list))
  (plot data #:x-label "fair" #:y-label "tough" #:title "delta 0.8"))

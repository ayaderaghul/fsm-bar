#lang racket

(provide (all-defined-out))
(require "automata.rkt")

;; CONFIGURATION
(define MAX-STATES# 10) ; create an automaton having up to 15 states

;; POPULATION
(define (build-random-population n)
  (define v (build-vector n (lambda (_) (make-random-automaton (+ 1 (random MAX-STATES#))))))
  (cons v v))

(define (population-payoffs population0)
  (define population (car population0))
  (for/list ([a population]) (automaton-payoff a)))

(define (match-up* population0 rounds-per-match delta)
  (define population (car population0))
  (population-reset population)
  (for ([i (in-range 0 (- (vector-length population) 1) 2)])
    (define p1 (vector-ref population i))
    (define p2 (vector-ref population (+ i 1)))
    (define-values (round-results a1 a2) (interact p1 p2 rounds-per-match delta))
    (vector-set! population i a1)
    (vector-set! population (+ i 1) a2))
  population0)

(define (population-reset a*)
  (for ([x a*][i (in-naturals)]) (vector-set! a* i (clone x))))

(define (regenerate population0 rate #:random (q #false))
  (match-define (cons a* b*) population0)
  (define probabilities (payoff->probabilities a*))
  [define substitutes   (choose-randomly probabilities rate #:random q)]
  (for ([i (in-range rate)][p (in-list substitutes)])
    (vector-set! a* i (clone (vector-ref b* p))))
  (shuffle-vector a* b*))

(define (mutate* population0 mutation)
  (define r (random 10))
  (cond [(= r 8) (mutate-n population0 mutation)]
        ;[(= r 9) (cross-over population0 mutation)]
        [else (mutate-c population0 mutation)]))

(define (mutate-c population0 mutation)
  (define p1 (car population0))
  (for ([i mutation])
    (define r (random (vector-length p1)))
    (define mutated (mutate (vector-ref p1 r)))
    (vector-set! p1 r mutated)))

(define (mutate-n population0 mutation)
  (define p1 (car population0))
  (for ([i mutation])
    (define r (random (vector-length p1)))
    (define mutated (make-random-automaton MAX-STATES#))
    (vector-set! p1 r mutated)))

(define (cross-over population0 mutation)
  (define p1 (car population0))
  (define l (vector-length p1))
  (for ([i mutation])
    (define r1 (random l))
    (define r2 (random l))
    (define au1 (vector-ref p1 r1))
    (define au2 (vector-ref p1 r2))
    (match-define (automaton c1 i1 pay1 table1) au1)
    (match-define (automaton c2 i2 pay2 table2) au2)
    (define l1 (vector-length table1))
    (define l2 (vector-length table2))
    (cond [(= l1 l2)
           (begin
             (define s (random l1))
             (define-values (h1 t1) (vector-split-at table1 s))
             (define-values (h2 t2) (vector-split-at table2 s))
             (define m1 (vector-append h1 t2))
             (define m2 (vector-append h2 t1))
             (vector-set! p1 r1 (automaton c1 i1 pay1 m1))
             (vector-set! p1 r2 (automaton c2 i2 pay2 m2)))]
          [else (mutate* population0 1)])))

(define (payoff->probabilities a*)
  (define payoffs (for/list ([x (in-vector a*)]) (automaton-payoff x)))
  (define total   (sum payoffs))
  (for/list ([p (in-list payoffs)]) (/ p total)))

;; UTILITIES
(define (shuffle-vector b a)
  ;; copy b into a
  (for ([x (in-vector b)][i (in-naturals)])
    (vector-set! a i x))
  ;; now shuffle a
  (for ([x (in-vector b)] [i (in-naturals)])
    (define j (random (add1 i)))
    (unless (= j i) (vector-set! a i (vector-ref a j)))
    (vector-set! a j x))
  (cons a b))

(define (sum l)
  (apply + l))

(define (relative-average l w) ; weighted mean
  (exact->inexact
   (/ (sum l)
      w (length l))))

(define (choose-randomly probabilities speed #:random (q #false))
  (define %s (accumulated-%s probabilities))
  (for/list ([n (in-range speed)])
    [define r (or q (random))]
    ;; population is non-empty so there will be some i such that ...
    (for/last ([p (in-naturals)] [% (in-list %s)] #:final (< r %)) p)))

(define (accumulated-%s probabilities)
  (let relative->absolute ([payoffs probabilities][so-far #i0.0])
    (cond
      [(empty? payoffs) '()]
      [else (define nxt (+ so-far (first payoffs)))
            (cons nxt (relative->absolute (rest payoffs) nxt))])))

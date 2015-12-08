#lang racket

(provide (all-defined-out))
(require "automata.rkt" "utilities.rkt")

(define MAX-STATES# 5)

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
(define p1 (car population0))
(for ([i mutation])
(define r (random (vector-length p1)))
(define mutated (mutate (vector-ref p1 r)))
(vector-set! p1 r mutated)))


(define (payoff->probabilities a*)
  (define payoffs (for/list ([x (in-vector a*)]) (automaton-payoff x)))
  (define total   (sum payoffs))
  (for/list ([p (in-list payoffs)]) (/ p total)))

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

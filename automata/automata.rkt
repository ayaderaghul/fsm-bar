#lang racket

(require "../configuration.rkt")
(provide (all-defined-out))

;; AUTOMATON
;; an automaton has 4 parts:
;; current state + initial state + the payoff it has so far + a table of its states
(struct automaton (current initial payoff table) #:transparent)

;; a state has 2 parts: the action of that state + the ending trajectories
;; (ie: if the opponent plays x, dispatch to state y)
(struct state (action dispatch) #:transparent)

(define (make-random-automaton states#)
  (define initial-current (random states#)) ; the initial state is set randomly
  (define (states*) (build-vector states# make-state))
  (define (make-state _) (state (random ACTIONS#) (transitions))) ; the action in each state is set randomly
  (define (transitions) (build-vector ACTIONS# make-transition))
  (define (make-transition _) (random (round (/ states# 3)))) ; the ending trajectories are set randomly, initially, it's set toward a smaller set of states
  (automaton initial-current initial-current 0 (states*)))

(define (clone a) ;; usually used to reset automaton to its default structure
  (match-define (automaton current c0 payoff table) a)
  (automaton c0 c0 0 table))

;; CLASSIC AUTOMATA
;; all highs: one state: playing High,
;; dispatching rules: L M H -> stays in state 0
(define (highs)
  (automaton 0 0 0 (vector (state HIGH (vector 0 0 0)))))
(define (mediums)
  (automaton 0 0 0 (vector (state MEDIUM (vector 0 0 0)))))
(define (lows)
  (automaton 0 0 0 (vector (state LOW (vector 0 0 0)))))
(define (accommodator)
  (automaton 1 1 0 (vector (state LOW (vector 2 1 0))
                            (state MEDIUM (vector 2 1 0))
                            (state HIGH (vector 2 1 0)))))
(define (tough)
  (automaton 0 0 0 (vector (state HIGH (vector 0 0 1))
                           (state HIGH (vector 1 1 2))
                           (state HIGH (vector 2 2 3))
                           (state MEDIUM (vector 0 3 0)))))
(define (bully)
  (automaton 0 0 0 (vector (state HIGH (vector 0 3 1))
                           (state HIGH (vector 1 1 2))
                           (state HIGH (vector 2 2 3))
                           (state MEDIUM (vector 1 3 0)))))

(define l (lows))
(define m (mediums))
(define h (highs))
(define t (tough))
(define b (bully))
(define a (accommodator))


;; (IMMUTABLE) MUTATION
;; mutable structure usually comes w unpleasant surprise
;; turn the automaton structure into a flattened list
(define (flatten-automaton au)
  (match-define (automaton current initial payoff states) au)
  (define len (vector-length states))
  (define body
    (for/list ([i (in-range len)])
      (match-define (state action dispatch) (vector-ref states i))
      (list action (vector->list dispatch))))
  (flatten (list initial body)))

;; recover automaton structure from a list
(define (recover au)
  (define-values (head body) (split-at au 1))
  (define s (/ (length body) STATE-LENGTH))
  (define (recover-body s a-body)
    (cond [(zero? s) '()]
          [else
           (define-values (first-state the-rest)
             (split-at a-body STATE-LENGTH))
           (define-values (action dispatch)
             (split-at first-state 1))
           (define result-state (state (car action) (list->vector dispatch)))
           (cons result-state (recover-body (- s 1) the-rest))]))
  (automaton (car head) (car head) 0 (list->vector (recover-body s body))))

(define (immutable-set a-list a-posn a-value)
  (define-values (head tail) (split-at a-list a-posn))
  (append head (list a-value) (drop tail 1)))

(define (mutate au)
  (define a (flatten-automaton au))
  (define r (random (length a)))
  (define s (quotient (length a) STATE-LENGTH))
  (recover
   (cond
    [(zero? r) (immutable-set a 0 (random s))]
    [(= 1 (modulo r STATE-LENGTH)) (immutable-set a r (random ACTIONS#))]
    [else (immutable-set a r (random s))])))


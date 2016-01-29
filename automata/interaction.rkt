#lang racket
(require "automata.rkt" math/matrix)
(provide (all-defined-out))

;; PAIR MATCH
;; interact with discount factor
;; return the after-matched pair
;; (after the match, the automaton becomes different,
;; it contains the information on its payoff and
;; it final states)
;; (used in simulation running)
(define (interact-d auto1 auto2 rounds-per-match delta pie)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2 round-results)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2])
              ([_ (in-range rounds-per-match)])
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2 pie))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (values n1 (+ payoff1 (* (expt delta _) p1))
              n2 (+ payoff2 (* (expt delta _) p2)))))
  (values (automaton new1 c1 p1 table1)
          (automaton new2 c2 p2 table2)))

;; interact with continual probability factor
;; since -d has become main module, -c is no longer supported
(define (interact-c auto1 auto2 rounds-per-match delta pie)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2 round-results)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2]
               [round-results '()])
              ([_ (in-range rounds-per-match)])
      #:final (> (random) delta)
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2 pie))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (define round-result (list p1 p2))
      (values n1 (+ payoff1 (* (- 1 delta) p1))
              n2 (+ payoff2 (* (- 1 delta) p2))
              (cons round-result round-results))))
  (values (reverse round-results)
          (automaton new1 c1 p1 table1)
          (automaton new2 c2 p2 table2)))

;; other modified interact
;; used to extract information
;; so it returns different output

;; interact-f returns full information
;; payoffs, states, strategies, payoff sequences
(define (interact-f auto1 auto2 rounds-per-match delta pie)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2 round-results state-results strat-results)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2]
               [round-results '()] [state-results (list (list current1 current2))]
	       [strat-results '()])
              ([_ (in-range rounds-per-match)])
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2 pie))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (define round-result (list p1 p2))
      (define state-result (list n1 n2))
      (define strat-result (list a1 a2))
      (values n1 (+ payoff1 (* (expt delta _) p1))
              n2 (+ payoff2 (* (expt delta _) p2))
              (cons round-result round-results)
              (cons state-result state-results)
              (cons strat-result strat-results))))
  (values (cons p1 p2) (reverse round-results) (reverse state-results) (reverse strat-results)))

;; interact-s returns less information
;; the final payoffs and the result of initial rounds
(define (interact-s auto1 auto2 rounds-per-match rounds-print delta pie)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2 round-results)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2]
               [round-results '()] )
              ([_ (in-range rounds-per-match)])
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2 pie))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (define round-result (list p1 p2))
      (values n1 (+ payoff1 (* (expt delta _) p1))
              n2 (+ payoff2 (* (expt delta _) p2))
              (cons round-result round-results)
              )))
  (values (cons (round-payoff p1 2) (round-payoff p2 2)) (take (reverse round-results) rounds-print)
))

;; interact returns simple information: final payoffs only
(define (interact auto1 auto2 rounds-per-match delta pie)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2]
               )
              ([_ (in-range rounds-per-match)])
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2 pie))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (values n1 (+ payoff1 (* (expt delta _) p1))
              n2 (+ payoff2 (* (expt delta _) p2))
              )))
  (cons (round-payoff p1 2) (round-payoff p2 2)))

;; UTILITIES
(define (round-payoff x n)
(define factor (expt 10 n))
(/ (round (* x factor))
   factor))

(define (payoff-matrix pie)
  (vector (vector (cons pie pie) (cons pie 5) (cons pie (- 10 pie)))
          (vector (cons 5 pie) (cons 5 5) (cons 0 0))
          (vector (cons (- 10 pie) pie) (cons 0 0) (cons 0 0))))

(define (payoff current1 current2 pie)
  (define payoffs (payoff-matrix pie))
  (vector-ref (vector-ref payoffs current1) current2))

;; MIXTURE INTERACTION
;; the previous part deals with interaction among pure strategies
;; but a mixture should be able to be thought of as one strategy also
;; so we write function to interact mixture
(define (payoff-table hori-lst vert-lst rounds-per-match delta pie)
  (for/list ([i (in-list vert-lst)])
    (for/list ([j (in-list hori-lst)])
      (car (interact i j rounds-per-match delta pie)))))

(define (interact-m hori-lst vert-lst weights rounds-per-match delta pie)
  (define hori-l (length hori-lst))
  (define vert-l (length vert-lst))
  (define mixture (payoff-table hori-lst hori-lst rounds-per-match delta pie))
  (define mixture-matrix
    (list->matrix hori-l hori-l (flatten mixture)))
  (define test (payoff-table hori-lst vert-lst rounds-per-match delta pie))
  (define test-matrix
    (list->matrix vert-l hori-l (flatten test)))
  (define w-row (list->matrix 1 hori-l weights))
  (define w-col (->col-matrix w-row))
  (cons
   (apply (lambda (x) (round-payoff x 2))
          (matrix->list (matrix* w-row mixture-matrix w-col)))
   (matrix->list (matrix* test-matrix w-col))))

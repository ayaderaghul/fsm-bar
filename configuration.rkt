#lang racket
(provide (all-defined-out))

;; AUTOMATON
(define LOW 0)
(define MEDIUM 1)
(define HIGH 2)
(define ACTIONS#  3) ; the number of strategies available for each player
(define STATE-LENGTH 4)
;; the state length is 4 bc each state has 4 information:
;; the strategy to be played at that state
;; and 3 dispatching rules: given that the opponent plays L M H, which state to jump to?

(define GAMMA 3) ; GAMMA < 5

(define PAYOFF-TABLE
  (vector (vector (cons GAMMA GAMMA) (cons GAMMA 5) (cons GAMMA (- 10 GAMMA)))
          (vector (cons 5 GAMMA) (cons 5 5) (cons 0 0))
          (vector (cons (- 10 GAMMA) GAMMA) (cons 0 0) (cons 0 0))))

(define AUTO-CODE "auto-code.nb") ; file name if exporting matha code

;; BASIC RUN
;; change the directory of output file here

(define lab1 "/Users/linhchi.nguyen/Dropbox/fsm-bar/grand/deltas/d/run3/")
(define disa "C:/Documents and Settings/linhchi.nguyen/My Documents/Dropbox/fsm-bar/grand/deltas/c/run1/")

(define MEAN (string-append "" "mean"))
(define RANK (string-append "" "rank"))
(define PIC (string-append "" "meanplot"))

;; change the simulation settings here
(define N 100)
(define CYCLES 10000)
(define SPEED 15)
(define ROUNDS 100)
(define DELTA .95)
(define MUTATION 1)

(define DATA-POINT 3) ; collect data every x cycles

;; RUN ACROSS DELTA
(define DELTAS-C (list .99 .97 .95 .93 0))
(define DELTAS (list 1 .95 .9 .85 0))

;; POPULATION
(define MAX-STATES# 50) ; generating automata w this number of states

;; SCAN
(define THRESHOLD 10) ; only collect data on automata that have at least 10 of its kind





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

(define PIE 3) ; pie < 5

(define AUTO-CODE "auto-code.nb") ; file name if exporting matha code

(define NTH-TREE 2) ;; nth order decision tree of an automaton

;; BASIC RUN
;; change the directory of output file here

(define lab1 "/Users/linhchi.nguyen/Dropbox/fsm-bar/grand/deltas/d/run3/")
(define disa "C:/Documents and Settings/linhchi.nguyen/My Documents/Dropbox/fsm-bar/grand/deltas/c/run1/")
(define home "./run/")
(define c "")

(define MEAN (string-append c "mean"))
(define RANK (string-append c "rank"))
(define PIC (string-append c "meanplot"))
(define RES (string-append c "respons"))

;; change the simulation settings here
(define N 100)
(define CYCLES 100)
(define SPEED 15)
(define ROUNDS 1)
(define DELTA .99)
(define MUTATION 1)

(define DATA-POINT 100) ; collect data every x cycles

;; RUN ACROSS DELTA
(define DELTAS-C (list .99 .97 .95 .93 0))
(define DELTAS (list .99 .95 .9 .85 .8 .7 .6 .4 .2 0))

;; PIES
(define PIES (list 0.5 1 2 3 4 4.5))

;; POPULATION
(define STATE# 30) ; generating automata w this number of states
(define STATES# (list 10 30 50 70 90))

;; SCAN
(define THRESHOLD 10) ; only collect data on automata that have at least 10 of its kind





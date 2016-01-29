#lang racket
(require "automata.rkt" "interaction.rkt")
(provide (all-defined-out))
;; PERSONALITY TEST
;; ok, so investigating decision tree doesnt work
;; plan B: in each cycle, rank* the population
;; take these automaton out
;; match them w themselves, Lows, Mediums, Highs
;; TOUGH personality: kind among themselves, resists Mediums, resists Highs, friend of bully
;; BULLY personality: kind among themselves, compromises Mediums, resists Highs, friend of tough
;; FAIR: be the best they can be among themselves, resists Highs, dominates bully, repulses tough
;; ACCOMMODATOR: submits to Highs, cooperates w Mediums, (exploit Lows)
;; HIGHS: fares badly among itself and Mediums, but exploits Lows & Accommodator

(define (questionaire auto-list rounds-per-match delta pie)
  (define test-kit (append auto-list (list l m h)))
  (payoff-table test-kit rounds-per-match delta pie))

(define (test-personality auto rounds-per-match delta pie)
  (define questionaire-result (questionaire (list auto) rounds-per-match delta pie))
  (match-define
   (list w-itself w-lows w-mediums w-highs) (first questionaire-result))
  (define fair-benchmark (third (third questionaire-result)))
  (match-define
   (cons high-potential low-pay) (interact h l rounds-per-match delta pie))
  (define high-pay (first (fourth questionaire-result)))
  (define medium-pay (first (third questionaire-result)))
  (define kindness (/ w-itself fair-benchmark))
  (define accommodation (/ high-pay high-potential))
  (define exploitation (/ w-lows high-potential))
  (define cooperation (/ medium-pay (+ w-itself 0.001)))
  (cons
   (cond [(= 1 kindness) (cond [(< accommodation .5) 'authentic-fair]
                               [else (cond [(> exploitation .5) 'accommodator]
                                           [else 'nice-accommodator])])]
         [(> kindness .5) (cond [(< cooperation .8) 'tough]
                                [(< cooperation 1) 'bullyish-tough]
                                [else (cond [(< accommodation .5) 'bully]
                                            [else 'low])])]
         [else (cond [(< cooperation .8) (cond [(> exploitation .6) 'high]
                                               [else 'lame])]
                     [else 'low])])
   (list "w-itself fair-benchmark w-fair w-highs highs-potential w-lows"
         w-itself fair-benchmark
         w-mediums w-highs
         high-potential w-lows)))

;; test personality of mixture
(define (test-personalities lst delta pie)
  (for/list ([i (in-list lst)])
    (test-personality i delta pie)))

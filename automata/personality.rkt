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
  (payoff-table test-kit test-kit rounds-per-match delta pie))

(define (read-test questionaire-result)
  (match-define
   (list w-itself w-lows w-mediums w-highs) (first questionaire-result))
  (define fair-benchmark (third (third questionaire-result)))
  (define highs-potential (second (fourth questionaire-result)))
  (define lows-benchmark (fourth (second questionaire-result)))
  (define highs-pay (first (fourth questionaire-result)))
  (define mediums-pay (first (third questionaire-result)))
  (define lows-pay (first (second questionaire-result)))
  (evaluate w-itself w-lows w-mediums w-highs
            lows-pay mediums-pay highs-pay
            lows-benchmark fair-benchmark highs-potential))

(define (evaluate w-itself w-lows w-mediums w-highs
                  lows-pay mediums-pay highs-pay
                  lows-benchmark fair-benchmark highs-potential)
  (define kindness (/ w-itself fair-benchmark))
  (define accommodation (/ highs-pay highs-potential))
  (define exploitation (/ w-lows highs-potential))
  (define cooperation (/ mediums-pay (+ w-itself 0.001)))
  (cons
   (cond [(= 1 kindness) (cond [(< accommodation .5) 'authentic-fair]
                               [else (cond [(> exploitation .5) 'accommodator]
                                           [else 'nice-accommodator])])]
         [(> kindness .5) (cond [(< cooperation .9) 'tough]
                                [(< cooperation 1) 'bullyish-tough]
                                [else (cond [(< accommodation .5) 'bully]
                                            [else 'low])])]
         [else (cond [(< cooperation .8) (cond [(> exploitation .6) 'high]
                                               [else 'lame])]
                     [else 'low])])
   (list "w-itself fair-benchmark mediums-pay highs-pay highs-potential w-lows"
         w-itself fair-benchmark
         mediums-pay highs-pay
         highs-potential w-lows)))

(define (test-auto auto rounds delta pie)
  (define questionaire-result (questionaire (list auto) rounds delta pie))
  (read-test questionaire-result))

(define (test-autos lst rounds-per-match delta pie)
  (for/list ([i (in-list lst)])
    (test-auto i rounds-per-match delta pie)))

;; test personality of mixture

(define (read-test-m questionaire-result)
  (match-define (list w-itself w-lows w-mediums w-highs
                      lows-pay mediums-pay highs-pay
                      lows-benchmark fair-benchmark highs-potential)
                questionaire-result)
  (evaluate w-itself w-lows w-mediums w-highs
            lows-pay mediums-pay highs-pay
            lows-benchmark fair-benchmark highs-potential))

(define (test-mixture mixture auto-numbers rounds delta pie)
  (define total (apply + auto-numbers))
  (define weights (for/list ([i (in-list auto-numbers)])
                    (/ i total)))
  (define questionaire-result (questionaire-m mixture weights rounds delta pie))
  (read-test-m questionaire-result))

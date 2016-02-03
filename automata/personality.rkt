#lang racket
(require "automata.rkt" "interaction.rkt" math/matrix)
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

(define (evaluate w-itself w-lows w-mediums w-highs
                  lows-pay mediums-pay highs-pay
                  lows-benchmark fair-benchmark highs-potential)
  (define kindness (/ w-itself fair-benchmark))
  (define accommodation (/ highs-pay highs-potential))
  (define exploitation (/ w-lows highs-potential))
  (define cooperation (/ mediums-pay (+ w-itself 0.001)))
  (cons
   (cond [(> kindness .995) (cond [(< accommodation .5) 'fair]
                               [else 'accommodator]
                                           )]
         [(> kindness .6) (cond [(< cooperation .9) 'tough]
                                [(< cooperation 1) 'bullyish-tough]
                                [else (cond [(< accommodation .5) 'bully]
                                            [else 'almost-accommodator])])] ;; almost or nice accommodator
         [else (cond [(< cooperation .8) (cond [(> exploitation .6) 'high]
                                               [else 'lame])]
                     [else 'low])])
   (list "w-itself fair-benchmark mediums-pay highs-pay highs-potential w-lows lows-pay"
         w-itself fair-benchmark
         mediums-pay highs-pay
         highs-potential w-lows lows-pay)))

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

(define (test-auto auto rounds delta pie)
  (define questionaire-result (questionaire (list auto) rounds delta pie))
  (read-test questionaire-result))

(define (test-autos lst rounds-per-match delta pie)
  (for/list ([i (in-list lst)])
    (test-auto i rounds-per-match delta pie)))
;; test personality of mixture

(define (questionaire-m auto-list weights rounds-per-match delta pie)
  (define test-kit (list l m h))
  (define len (length auto-list))
  (define mixture (payoff-table auto-list auto-list rounds-per-match delta pie))
  (define w-lmh (payoff-table test-kit auto-list rounds-per-match delta pie))
  (define lmh-pay (payoff-table auto-list test-kit rounds-per-match delta pie))
  (define mixture-matrix (list->matrix len len (flatten mixture)))
  (define w-lmh-matrix (list->matrix len 3 (flatten w-lmh)))
  (define lmh-pay-matrix (list->matrix 3 len (flatten lmh-pay)))
  (define weight-row (list->matrix 1 len weights))
  (define weight-col (->col-matrix weight-row))
  (match-define (cons highs-potential lows-pay) (interact h l rounds-per-match delta pie))
  (define fair-benchmark (car (interact m m rounds-per-match delta pie)))
  (append
   (matrix->list (matrix* weight-row mixture-matrix weight-col))
   (matrix->list (matrix* weight-row w-lmh-matrix))
   (matrix->list (matrix* lmh-pay-matrix weight-col))
   (list lows-pay fair-benchmark highs-potential)
   ))
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

;; test a simulation
(define (test-simulation lst data-point rounds delta pie)
  (define (test-au mixture posn rounds delta pie)
    (for/list ([i (in-list mixture)])
      (hash
       (first (test-auto (car i) rounds delta pie))
       (list posn (cdr i)))))
  (define len (length lst))
  (define xs (build-list len (lambda (x) (* data-point x))))
  (flatten
   (for/list ([i (in-list lst)]
              [j (in-list xs)])
     (if (list? i) (test-au i j rounds delta pie)
         (hash 'nothing (list 0 0))))))

(define (test-simulation-m lst data-point rounds delta pie)
  (define (test-mix mix posn rounds delta pie)
    (define autos (map car mix))
    (define auto-numbers (map cdr mix))
    (hash
     (first (test-mixture autos auto-numbers rounds delta pie))
     (list posn (apply + auto-numbers))))
  (define len (length lst))
  (define xs (build-list len (lambda (x) (* data-point x))))
  (for/list ([i (in-list lst)]
             [j (in-list xs)])
    (if (list? i) (test-mix i j rounds delta pie)
        (hash 'nothing (list 0 0)))))

;; combine both method (return both test-auto and test-mixture at the same time)
(define (test lst data-point rounds delta pie)
  (define (test-au mixture posn rounds delta pie)
    (for/list ([i (in-list mixture)])
      (hash
       (first (test-auto (car i) rounds delta pie))
       (list posn (cdr i)))))
  (define (test-mix mixture posn rounds delta pie)
    (define autos (map car mixture))
    (define auto-numbers (map cdr mixture))
    (hash
     (first (test-mixture autos auto-numbers rounds delta pie))
     (list posn (apply + auto-numbers))))
  (define len (length lst))
  (define xs (build-list len (lambda (x) (* data-point x))))
  (define (test* f rounds delta pie)
    (for/list ([i (in-list lst)]
               [j (in-list xs)])
      (if (list? i) (f i j rounds delta pie)
          (hash 'nothing (list 0 0)))))
  (define test-result
    (flatten
     (test* test-au rounds delta pie)))
  (define test-result-m
    (test* test-mix rounds delta pie))
  (list test-result test-result-m))

#lang racket

(provide (all-defined-out))
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

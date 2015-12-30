#lang racket
(require "automata.rkt" "csv.rkt" 2htdp/batch-io)
(provide rank scan-oneshot-types scan-mediums-highs scan-mediums-tough)

;; CONFIGURATION
(define THRESHOLD 10) ; only collect data on automata that have at least 10 of its kind

;; SCAN
(define (initial-action au)
  (match-define (automaton current initial payoff states) au)
  (match-define (state action dispatch) (vector-ref states initial))
  action)

;; scan* is cheaper because it doesnt flatten automata before ranking
;; but the output is very hard to read
(define (scan* population)
  (define p0 (vector->list (car population)))
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   p0))

(define (scan population)
  (define p0 (vector->list (car population)))
  (define (hash-update* an-auto a-hash)
    (hash-update a-hash an-auto add1 0))
  (define init-hash (hash))
  (foldl
   (lambda (au h) (hash-update* (flatten-automaton au) h))
   init-hash
   p0))

(define (rank population)
  (define a-hash (scan population))
  (for/hash ([(k v) (in-hash a-hash)]
             #:when (< THRESHOLD v))
    (values k v)))

(define (rank* population)
  (define a-hash (scan* population))
   (for/hash ([(k v) (in-hash a-hash)]
              #:when (< THRESHOLD v))
     (values k v)))

(define (scan-initials population)
  (define p0 (vector->list (car population)))
  (foldl
   (lambda (au h)
     (hash-update h (initial-action au) add1 0))
   (hash)
   p0))

(define (hash-ref* a-hash a-key)
  (if (hash-has-key? a-hash a-key)
      (hash-ref a-hash a-key)
      0))

;; SCAN FOR DIFFERENT TESTS
(define (scan-oneshot-types population)
  (let ([ranking (scan-initials population)])
    (list
     (hash-ref* ranking 0)
     (hash-ref* ranking 1))))

(define (scan-mediums-highs population)
  (let ([ranking (scan population)])
    (list
     (hash-ref* ranking (list 0 1 0 0 0))
     (hash-ref* ranking (list 0 2 0 0 0)))))

(define (scan-mediums-tough population)
  (let ([ranking (scan population)])
    (list
     (hash-ref* ranking (list 0 1 0 0 0))
     (hash-ref* ranking (list 0 2 0 0 1 2 1 1 2 2 2 2 3 1 0 3 0)))))


;; NEED FIXING (USING RANK*)
(define (top t population)
  (let* ([flattened (map car (rank population))]
         [automaton (map (lambda (au)
                           (apply automaton au)) (take flattened t))])
    (for/list ([i t])
      (eval
       (list 'define (x->ax i)
             (list-ref automaton i))))))

(define (x->ax x)
  (string->symbol (string-append "a" (number->string x))))

(define (generate-ax a-list)
  (map x->ax a-list))

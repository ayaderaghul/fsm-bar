#lang racket
(require "configuration.rkt" "./automata/automata.rkt" "./automata/personality.rkt" "utilities.rkt")
(provide rank rank* scan scan* scan-char scan-char-m )

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

;; scan for types (personality)

;(define (how-manys lst h)
;(for/sum ([i (in-list lst)])
;(how-many i h)))

(define (scan-char lst rounds delta pie)
  (define autos (map car lst))
  (define auto-numbers (map cdr lst))
  (define result
    (for/list ([i (in-list autos)]
               [j (in-list auto-numbers)])
      (cons (first (test-auto i rounds delta pie)) j)))
  (foldl
   (lambda (entry h) (hash-update h (car entry)
                             (lambda (x) (+ (cdr entry) x))
                             0))
   (hash)
   result))

(define (scan-char-m lst rounds delta pie)
(define autos (map car lst))
(define auto-numbers (map cdr lst))
(hash 
(first (test-mixture autos auto-numbers rounds delta pie))
(apply + auto-numbers)))


;; SCAN FOR DIFFERENT TESTS
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

(define (scan-mediums-accom population)
  (let ([ranking (scan population)])
    (list
     (hash-ref* ranking (list 0 1 0 0 0))
     (hash-ref* ranking (list 1 0 2 1 0 1 2 1 0 2 2 1 0)))))

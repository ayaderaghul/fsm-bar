#lang racket
(require "automata.rkt" "csv.rkt" 2htdp/batch-io)
(provide (all-defined-out))

;; SCAN

(define (initial-action au)
  (match-define (automaton current initial payoff states) au)
  (match-define (state action dispatch) (vector-ref states initial))
  action)


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

(define THRESHOLD 10)

(define (rank population)
  (define a-hash (scan population))
  (for/hash ([(k v) (in-hash a-hash)]
             #:when (< THRESHOLD v))
    (values k v)))

(define (rank* population)
  (define a-hash (time (scan* population)))
  (time
   (for/hash ([(k v) (in-hash a-hash)]
              #:when (< THRESHOLD v))
     (values k v))))

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

(define (scan-4-types population)
  (let ([ranking (scan population)])
    (list
     (hash-ref* ranking (list 0 1 0 0 0))
     (hash-ref* ranking (list 0 2 0 0 0)))))

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

;; OUT
;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

(define (out-mean data)
  (out-data "mean" (map list data)))

(define (out-rank day data)
  (out-data "rank" (append (list (list day)
                                 (map list data)))))

#lang racket
(require "configuration.rkt" "automata.rkt")
(provide responses response-tree print-response-tree response-states print-response-states)

;; INVESTIGATE AUTOMATON
;; decision tree
;; 0 order decision tree: what (3) states does the initial state tell you to jump to
;; 1-order decision tree: what (3^2) states does the 0-order-state tell you to jump to
(define (nth-decision-tree n auto)
  (match-define (automaton c0 i0 p0 table0) auto)
  (define d0 (state#->#s i0 table0))
  (define-values (dn result)
    (for/fold ([dx d0]
               [ds (cons d0 (cons (list i0) '()))])
              ([_ n])
      (define d-next (states#->##s dx table0))
      (values d-next (cons d-next ds))))
  (reverse result))

(define (states#->##s _ table)
  (flatten (for/list ([i (in-list _)]) (state#->#s i table))))
(define (state#->#s _ table)
  (match-define (state a d) (vector-ref table _))
  (vector->list d))

;; how does the automaton response to a L, M, H move?
;; utilities:
(define (vector-first a-vec)
  (vector-ref a-vec 0))
(define (vector-second a-vec)
  (vector-ref a-vec 1))
(define (vector-third a-vec)
  (vector-ref a-vec 2))

(define (how-many-l? lst)
  (count zero? lst))
(define (one? x) (= 1 x))
(define (two? x) (= 2 x))
(define (how-many-m? lst)
  (count one? lst))
(define (how-many-h? lst)
  (count two? lst))
(define (count-lmh lst)
  (list (count zero? lst) (count one? lst) (count two? lst)))

(define (responses auto)
  (match-define (automaton c0 i0 p0 table0) auto)
  (define states (vector->list table0))
  (responses* states states))

(define (responses* states table)
  (define dispatches (map state-dispatch states))
  (define l->states# (map vector-first dispatches))
  (define m->states# (map vector-second dispatches))
  (define h->states# (map vector-third dispatches))
  (define (state#->action _) (state-action (list-ref table _)))
  (define l->actions (map state#->action l->states#))
  (define m->actions (map state#->action m->states#))
  (define h->actions (map state#->action h->states#))
  (list (count-lmh l->actions) (count-lmh m->actions) (count-lmh h->actions)))

;; the simplest accommodator response distribution is this:
;; ((0 0 3) (0 3 0) (3 0 0))
;; it responds to l by 3 h
;; it responds to m by 3 m
;; it responds to h by 3 l

;; investigate only n-order decision tree
;; CAKE
(define (print-char str n char)
  (printf str (make-string n char))
  (newline))
(define (make-histogram lst)
  (match-define (list l m h) lst)
  (print-char "L: ~a" l #\|)
  (print-char "M: ~a" m #\|)
  (print-char "H: ~a" h #\|)
  (print-char "~a" 10 #\-))
(define (make-histograms lst)
  (map make-histogram lst))

(define (response-tree n auto)
  (match-define (automaton c0 i0 p0 table0) auto)
  (define states (vector->list table0))
  (define tree# (nth-decision-tree n auto))
  (define (state#->state _) (list-ref states _))
  (define (states#->states lst) (map state#->state lst))
  (define tree (map states#->states tree#))
  (define (respond lst) (responses* lst states))
  (map respond tree))

(define (print-response-tree auto)
(define resp (response-tree 2 auto))
(fourth resp))
(define (plot-response-branch auto)
  (define resp (response-tree 2 auto))
  (make-histograms (fourth resp)))

(define (print-response-states n auto)
  (match-define (automaton c0 i0 p0 table0) auto)
  (define states (vector->list table0))
  (define tree# (remove-duplicates (flatten (nth-decision-tree n auto))))
  (define (state#->state _) (list-ref states _))
  (define tree (map state#->state tree#))
  (define distribution (responses* tree states))
  (map make-histogram distribution))

(define (response-states n auto)
  (match-define (automaton c0 i0 p0 table0) auto)
  (define states (vector->list table0))
  (define tree# (remove-duplicates (flatten (nth-decision-tree n auto))))
  (define (state#->state _) (list-ref states _))
  (define tree (map state#->state tree#))
  (responses* tree states))


#lang racket
(require "configuration.rkt") 
(require "cake.rkt")
(provide (all-defined-out))


;; AUTOMATON
;; an automaton has 4 parts:
;; current state + initial state + the payoff it has so far + a table of its states
(struct automaton (current initial payoff table) #:transparent)
;; a state has 2 parts: the action of that state + the ending trajectories
;; (ie: if the opponent plays x, dispatch to state y)
(struct state (action dispatch) #:transparent)

(define (make-random-automaton states#)
  (define initial-current (random states#)) ; the initial state is set randomly
  (define (states*) (build-vector states# make-state))
  (define (make-state _) (state (random ACTIONS#) (transitions))) ; the action in each state is set randomly
  (define (transitions) (build-vector ACTIONS# make-transition))
  (define (make-transition _) (random states#)) ; the ending trajectories are set randomly
  (automaton initial-current initial-current 0 (states*)))

(define (clone a)
  (match-define (automaton current c0 payoff table) a)
  (automaton c0 c0 0 table))

;; CLASSIC AUTOMATA
;; all highs: one state: playing High,
;; dispatching rules: L M H -> stays in state 0
(define (highs)
  (automaton 0 0 0 (vector (state HIGH (vector 0 0 0)))))
(define (mediums)
  (automaton 0 0 0 (vector (state MEDIUM (vector 0 0 0)))))
(define (lows)
  (automaton 0 0 0 (vector (state LOW (vector 0 0 0)))))
(define (accommodator)
  (automaton 1 1 0 (vector (state LOW (vector 2 1 0))
                            (state MEDIUM (vector 2 1 0))
                            (state HIGH (vector 2 1 0)))))
(define (tough)
  (automaton 0 0 0 (vector (state HIGH (vector 0 0 1))
                           (state HIGH (vector 1 1 2))
                           (state HIGH (vector 2 2 3))
                           (state MEDIUM (vector 0 3 0)))))
(define (bully)
  (automaton 0 0 0 (vector (state HIGH (vector 0 3 1))
                           (state HIGH (vector 1 1 2))
                           (state HIGH (vector 2 2 3))
                           (state MEDIUM (vector 1 3 0)))))

;; PAIR MATCH
(define (interact-d auto1 auto2 rounds-per-match delta gamma)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2 round-results)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2]
               [round-results '()])
              ([_ (in-range rounds-per-match)])
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2 gamma))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (define round-result (list p1 p2))
      (values n1 (+ payoff1 (* (expt delta _) p1))
              n2 (+ payoff2 (* (expt delta _) p2))
              (cons round-result round-results))))
  (values (reverse round-results)
          (automaton new1 c1 p1 table1)
          (automaton new2 c2 p2 table2)))


(define (interact-c auto1 auto2 rounds-per-match delta gamma)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2 round-results)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2]
               [round-results '()])
              ([_ (in-range rounds-per-match)])
      #:final (> (random) delta)
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2 gamma))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (define round-result (list p1 p2))
      (values n1 (+ payoff1 (* (- 1 delta) p1))
              n2 (+ payoff2 (* (- 1 delta) p2))
              (cons round-result round-results))))
  (values (reverse round-results)
          (automaton new1 c1 p1 table1)
          (automaton new2 c2 p2 table2)))

(define (interact auto1 auto2 rounds-per-match delta gamma)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2 round-results state-results)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2]
               [round-results '()] [state-results (list (list current1 current2))])
              ([_ (in-range rounds-per-match)])
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2 gamma))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (define round-result (list p1 p2))
      (define state-result (list n1 n2))
      (values n1 (+ payoff1 (* (expt delta _) p1))
              n2 (+ payoff2 (* (expt delta _) p2))
              (cons round-result round-results)
              (cons state-result state-results))))
  (values (reverse round-results) (reverse state-results)))

(define (payoff-table gamma)
  (vector (vector (cons gamma gamma) (cons gamma 5) (cons gamma (- 10 gamma)))
          (vector (cons 5 gamma) (cons 5 5) (cons 0 0))
          (vector (cons (- 10 gamma) gamma) (cons 0 0) (cons 0 0))))

(define (payoff current1 current2 gamma)
  (define payoffs (payoff-table gamma))
  (vector-ref (vector-ref payoffs current1) current2))
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
  (responses-h states states))

(define (responses-h states table)
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

(define (core-responses n auto)
  (match-define (automaton c0 i0 p0 table0) auto)
  (define states (vector->list table0))
  (define tree# (nth-decision-tree n auto))
  (define (state#->state _) (list-ref states _))
  (define (states#->states lst) (map state#->state lst))
  (define tree (map states#->states tree#))
  (define (respond lst) (responses-h lst states))
  (map respond tree))

(define (core-responses-2 n auto)
  (match-define (automaton c0 i0 p0 table0) auto)
  (define states (vector->list table0))
  (define tree# (remove-duplicates (flatten (nth-decision-tree n auto))))
  (define (state#->state _) (list-ref states _))
  (define tree (map state#->state tree#))
  (define distribution (responses-h tree states))
(map (lambda (x) (apply make-histogram x)) distribution)
)

(define (core-responses-3 n auto)
  (match-define (automaton c0 i0 p0 table0) auto)
  (define states (vector->list table0))
  (define tree# (remove-duplicates (flatten (nth-decision-tree n auto))))
  (define (state#->state _) (list-ref states _))
  (define tree (map state#->state tree#))
  (responses-h tree states))



;; (IMMUTABLE) MUTATION
;; turn the automaton structure into a flattened list
(define (flatten-automaton au)
  (match-define (automaton current initial payoff states) au)
  (define l (vector-length states))
  (define body
    (for/list ([i (in-range l)])
      (match-define (state action dispatch) (vector-ref states i))
      (list action (vector->list dispatch))))
  (flatten (list initial body)))

;; recover automaton structure from a list
(define (recover au)
  (define-values (head body) (split-at au 1))
  (define s (/ (length body) STATE-LENGTH))
  (define (recover-body s a-body)
    (cond [(zero? s) '()]
          [else
           (define-values (first-state the-rest)
             (split-at a-body STATE-LENGTH))
           (define-values (action dispatch)
             (split-at first-state 1))
           (define result-state (state (car action) (list->vector dispatch)))
           (cons result-state (recover-body (- s 1) the-rest))]))
  (automaton (car head) (car head) 0 (list->vector (recover-body s body))))

(define (immutable-set a-list a-posn a-value)
  (define-values (head tail) (split-at a-list a-posn))
  (append head (list a-value) (drop tail 1)))

(define (mutate au)
  (define a (flatten-automaton au))
  (define r (random (length a)))
  (define s (quotient (length a) STATE-LENGTH))
  (recover
   (cond
    [(zero? r) (immutable-set a 0 (random s))]
    [(= 1 (modulo r STATE-LENGTH)) (immutable-set a r (random ACTIONS#))]
    [else (immutable-set a r (random s))])))

;; EXPORT MATHA CODE OF THE AUTOMATON
(define (generate-state-code table)
  (define l (vector-length table))
  (define state-numbers (vector-map state-action table))
  (define state-labels
    (vector-map (lambda (x)
                  (cond ([zero? x] "L")
                        ([= 1 x] "M")
                        ([= 2 x] "H")))
         state-numbers))
  (define state-code
    (apply string-append
           (add-between
            (for/list ([i l])
              (string-append (number->string i) " -> Placed[\"~a\", Center]"))
            ", ")))
  (apply format
         (list* state-code (vector->list state-labels))))

(define (scan-duplicate dispatch)
  (match-define (vector a1 a2 a3) dispatch)
  (cond [(= a1 a2 a3) (list "\"L,M,H\"" "\"L,M,H\"" "\"L,M,H\"")]
        [(= a1 a2) (list "\"L,M\"" "\"L,M\"" "\"H\"")]
        [(= a1 a3) (list "\"L,H\"" "\"M\"" "\"L,H\"")]
        [(= a2 a3) (list "\"L\"" "\"M,H\"" "\"M,H\"")]
        [else (list "\"L\"" "\"M\"" "\"H\"")]))

(define (generate-dispatch-code state# dispatch)
  (define l (vector-length dispatch))
  (define ending (scan-duplicate dispatch))
  (remove-duplicates
   (for/list ([i l])
     (string-append
      "Labeled["
      (number->string state#)
      " -> "
      (number->string (vector-ref dispatch i))
      ", "
      (list-ref ending i)
      "]"))))

(define (generate-dispatch-codes table)
  (define dispatches (vector-map state-dispatch table))
  (define dispatch-code
    (for/list ([i (vector-length dispatches)])
      (generate-dispatch-code i (vector-ref dispatches i))))
  (apply string-append (add-between (flatten dispatch-code) ", ")))

(define (generate-matha-code au name)
  (match-define (automaton current initial payoff states) au)
  (string-append
   "SetDirectory[NotebookDirectory[]];
   VertexCircle[{xc_, yc_}, name_, {w_, h_}] := Disk[{xc, yc}, .1];\n"
   name "Graph =\n"
   "   Graph[{-1 -> " (number->string initial) " ,\n"
   (generate-dispatch-codes states)
   "     },\n"
   "   EdgeShapeFunction -> \n"
   "    GraphElementData[\"EdgeShapeFunction\", \"FilledArrow\"],\n"
   "   VertexStyle -> LightGray,\n"
   "   VertexShapeFunction -> VertexCircle,\n"
   "   VertexLabels -> {" (generate-state-code states) "}\n"
   "   ];\n"
   ;; "G = Graphics[{White, Disk[{0, 0}, 0.2]}];\n"
   "S = Show[" name "Graph]\n"
   "(*Export[\"" name ".png\",S]*)\n \n"))

(define (export-matha-code au name)
  (with-output-to-file AUTO-CODE
    (lambda () (printf (generate-matha-code au name)))
    #:exists 'append))

(define (export-matha-codes a-list name)
  (for ([i (length a-list)])
    (export-matha-code (list-ref a-list i)
                       (string-append (symbol->string name)
                                      (number->string i)))))

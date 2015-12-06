#lang racket
(provide (all-defined-out))

(define LOW 0)
(define MEDIUM 1)
(define HIGH 2)
(define actions#  3)
(struct automaton (current initial payoff table) #:transparent)
(struct state (action dispatch) #:transparent)

(define (make-random-automaton states#)
  (define initial-current (random states#))
  (define (states*) (build-vector states# make-state))
  (define (make-state _) (state (random actions#) (transitions)))
  (define (transitions) (build-vector actions# make-transition))
  (define (make-transition _) (random states#))
  (automaton initial-current initial-current 0 (states*)))

(define (highs p0)
  (automaton 0 0 p0 (vector (state HIGH (vector 0 0 0)))))

(define (mediums p0)
  (automaton 0 0 p0 (vector (state MEDIUM (vector 0 0 0)))))
(define (lows p0)
  (automaton 0 0 p0 (vector (state LOW (vector 0 0 0)))))

(define (clone a)
  (match-define (automaton current c0 payoff table) a)
  (automaton c0 c0 0 table))

(define (interact auto1 auto2 rounds-per-match delta)
  (match-define (automaton current1 c1 payoff1 table1) auto1)
  (match-define (automaton current2 c2 payoff2 table2) auto2)
  (define-values (new1 p1 new2 p2 round-results)
    (for/fold ([current1 current1] [payoff1 payoff1]
               [current2 current2] [payoff2 payoff2]
               [round-results '()])
              ([_ (in-range rounds-per-match)])
      (match-define (state a1 v1) (vector-ref table1 current1))
      (match-define (state a2 v2) (vector-ref table2 current2))
      (match-define (cons p1 p2) (payoff a1 a2))
      (define n1 (vector-ref v1 a2))
      (define n2 (vector-ref v2 a1))
      (define round-result (list p1 p2))
      (values n1 (+ payoff1 (* (expt delta _) p1))
              n2 (+ payoff2 (* (expt delta _) p2))
              (cons round-result round-results))))
  (values (reverse round-results) (automaton new1 c1 p1 table1) (automaton new2 c2 p2 table2)))

(define PAYOFF-TABLE
  (vector (vector (cons 2 2) (cons 2 5) (cons 2 8))
          (vector (cons 5 2) (cons 5 5) (cons 0 0))
          (vector (cons 8 2) (cons 0 0) (cons 0 0))))

(define (payoff current1 current2)
  (vector-ref (vector-ref PAYOFF-TABLE current1) current2))

(define (flatten-automaton au)
  (match-define (automaton current initial payoff states) au)
  (define l (vector-length states))
  (define body
    (for/list ([i l])
      (match-define (state action dispatch) (vector-ref states i))
      (list action (vector->list dispatch))))
  (flatten (list initial body)))

(define STATE-LENGTH 4)

(define (recover au)
  (define s (/ (- (length au) 1) STATE-LENGTH))
  (define body (drop au 1))
  (define recovered-body
    (for/vector ([i s])
      (state (list-ref body (* STATE-LENGTH i))
             (list->vector (take (drop body (+ 1 (* STATE-LENGTH i))) actions#)))))
  (automaton (first au) (first au) 0 recovered-body))

(define (immutable-set a-list a-posn a-value)
  (define head (take a-list a-posn))
  (define tail (drop a-list (+ 1 a-posn)))
  (append head (list a-value) tail))

(define (mutate au)
  (define a (flatten-automaton au))
  (define r (random (length a)))
  (define s (/ (- (length a) 1) STATE-LENGTH))
  (recover
   (cond
    [(zero? r) (immutable-set a 0 (random s))]
    [(= 1 (modulo r STATE-LENGTH)) (immutable-set a r (random actions#))]
    [else (immutable-set a r (random s))])))


;; EXPORT MATHA CODE

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
        [(= a2 a3) (list "\"L\"" "\"M,H\"" "\"M,H\"")]))


(define (generate-dispatch-code state-i dispatch)
  (define l (vector-length dispatch))
  (define ending (scan-duplicate dispatch))
  (remove-duplicates
   (for/list ([i l])
     (string-append
      "Labeled["
      (number->string state-i)
      " -> "
      (number->string (vector-ref dispatch i))
      ", "
      (list-ref ending i)
      "] \n"))))

(define (generate-dispatch-codes table)
  (define dispatches (vector-map state-dispatch table))
  (define dispatch-code
    (for/list ([i (vector-length dispatches)])
      (generate-dispatch-code i (vector-ref dispatches i))))
  (apply string-append (add-between (flatten dispatch-code) ", ")))

(define (generate-matha-code au name)
  (match-define (automaton current initial payoff states) au)
  (string-append
   "VertexCircle[{xc_, yc_}, name_, {w_, h_}] := Disk[{xc, yc}, .1];\n"
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
   "G = Graphics[{White, Disk[{0, 0}, 0.2]}];\n"
   "Show[" name "Graph, G]\n"
   "(*Export[\"" name ".png\",S]*)\n \n"))

(define (export-matha-code au name)
  (with-output-to-file "auto-code.nb"
    (lambda () (printf (generate-matha-code au name)))
    #:exists 'append))

(define (export-matha-codes a-list name)
  (for ([i (length a-list)])
    (export-matha-code (list-ref a-list i)
                       (string-append (symbol->string name)
                                      (number->string i)))))

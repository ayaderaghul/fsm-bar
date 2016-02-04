#lang racket
(require "../configuration.rkt" "automata.rkt")
(provide export-mathas export-matha-codes)

;; EXPORT MATHA CODE OF THE AUTOMATON
(define (number->action x)
  (cond ([zero? x] "\"L\"")
        ([= 1 x] "\"M\"")
        ([= 2 x] "\"H\"")))

(define (generate-state a-state)
  (match-define (state a d) a-state)
  (string-append
   (string-join
    (list
     (number->action a)
     (string-join (map number->string (vector->list d)) ", "
                  #:before-first "{"
                  #:after-last "}"))
    ", "
    #:before-first "{"
    #:after-last "}")))

(define (generate-auto auto name)
  (match-define (automaton c i p table) auto)
  (string-append name " = {" (number->string c) ", "
    (string-join
     (vector->list (vector-map generate-state table))
     ", "
     #:before-first "{"
     #:after-last "}}\n")))

(define (export-mathas a-list name)
  (with-output-to-file AUTO-CODE
    (lambda () (printf (generate-autos a-list name)))
    #:exists 'replace))

(define (generate-autos a-list name)
  (string-join
   (for/list ([i (length a-list)]
              [j (in-list a-list)])
     (generate-auto j
                    (string-append (symbol->string name) (number->string i))))
   "\n"))



;; EXPORT MATHA CODE OF THE AUTOMATON GRAPH
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

(define (export-matha-codes a-list name)
  (with-output-to-file AUTO-CODE
    (lambda () (printf (generate-matha-codes a-list name)))
    #:exists 'append))

(define (generate-matha-codes a-list name)
(string-join
  (for/list ([i (length a-list)] [j (in-list a-list)])
(generate-matha-code j
       (string-append (symbol->string name)
                                      (number->string i))))
"\n \n"
))


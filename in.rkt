#lang racket
(provide (all-defined-out))
(require 2htdp/batch-io plot)

;; IMPORT
(define (load-data csv-file)
  [define strings (read-csv-file csv-file)]
  [define l (length strings)]
  [define-values (data)
    (for/fold ([data '()])
              ([i (in-range l)])
      [define datum (apply string->number (list-ref strings i))]
      (values (cons datum data)))]
  (reverse data))

(define (pack-coors a-list)
  [define l (length a-list)]
  (for/list ([i (in-range (/ l 2))])
    (list
     (list-ref a-list (* 2 i))
     (list-ref a-list (add1 (* 2 i))))))

(define (load-dynamic csv-file)
  (lines (pack-coors (load-data csv-file))))

(define (load-dynamics file-list)
  [define l (length file-list)]
  (for/list ([i (in-range l)])
    (load-dynamic (list-ref file-list i))))




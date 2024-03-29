#lang racket/base

(require racket/file
         racket/list
         rackunit)

(define m (- (/ 1 3)))
(define b 0)

(define (solve grid)
  (for/sum ([y (range (length grid))])
      (let ([row (list-ref grid y)]
            [x (get-x-coord (- y) m b)])
        (if (list-ref row (modulo x (length row))) 1 0))))

(define (get-x-coord y m b)
  (/ (- y b) m))

(define (get-input file-path)
  (for/list ([line (file->lines file-path)])
    (for/list ([char (string->list line)])
      (eqv? char #\#))))

(check-eqv? (solve (get-input "input.txt")) 282)

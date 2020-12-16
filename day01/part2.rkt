#lang racket/base

(require racket/file
         racket/match
         rackunit)

(define (solve** x y ls)
  (match ls
    [(list z rest ...)
     (if (= 2020 (+ x y z)) (* x y z) (solve** x y rest))]
    [else #f]))

(define (solve* x ls)
  (match ls
    [(list y rest ...)
     (let ([result (solve** x y rest)])
       (if result result (solve* x rest)))]
    [else #f]))

(define (solve entries)
  (match entries
    [(list x rest ...)
     (let ([result (solve* x rest)])
       (if result result (solve rest)))]
    [else #f]))

(check-eqv? (solve (map string->number (file->lines "input.txt"))) 103927824)

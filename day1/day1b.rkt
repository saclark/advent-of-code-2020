#lang racket/base

(require racket/file
         racket/match)

(define (day1** x y ls)
  (match ls
    [(list z rest ...)
     (if (= 2020 (+ x y z)) (* x y z) (day1** x y rest))]
    [else #f]))

(define (day1* x ls)
  (match ls
    [(list y rest ...)
     (let ([result (day1** x y rest)])
       (if result result (day1* x rest)))]
    [else #f]))

(define (day1 entries)
  (match entries
    [(list x rest ...)
     (let ([result (day1* x rest)])
       (if result result (day1 rest)))]
    [else #f]))

(day1 (map string->number (file->lines "day1-input.txt")))

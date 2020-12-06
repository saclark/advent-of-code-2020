#lang racket/base

(require racket/file
         racket/match)

(define (day1 entries)
  (match entries
    [(list x xs ...)
     (let ([result (day1* x xs)])
       (if result result (day1 xs)))]
    [else #f]))

(define (day1* x xs)
  (match xs
    [(list y rest ...)
     (if (= 2020 (+ x y))
         (* x y)
         (day1* x rest))]
    [else #f]))

(day1 (map string->number (file->lines "day1-input.txt")))

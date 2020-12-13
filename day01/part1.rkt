#lang racket/base

(require racket/file
         racket/match)

(define (solve entries)
  (match entries
    [(list x xs ...)
     (let ([result (solve* x xs)])
       (if result result (solve xs)))]
    [else #f]))

(define (solve* x xs)
  (match xs
    [(list y rest ...)
     (if (= 2020 (+ x y))
         (* x y)
         (solve* x rest))]
    [else #f]))

(solve (map string->number (file->lines "input.txt")))

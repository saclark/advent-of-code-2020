#lang typed/racket/base

(require racket/file
         racket/match
         typed/rackunit)

(: solve (-> (Listof Integer) (U Integer #f)))
(define (solve entries)
  (: solve-aux (-> Integer (Listof Integer) (U Integer #f)))
  (define (solve-aux x xs)
    (match xs
      [(list y rest ...) (if (= 2020 (+ x y)) (* x y) (solve-aux x rest))]
      [else #f]))
  (match entries
    [(list x xs ...) (let ([result (solve-aux x xs)])
                       (if result result (solve xs)))]
    [else #f]))

(: parse-input (-> String (Listof Integer)))
(define (parse-input input-file-path)
  (map string->integer (file->lines input-file-path)))

(: string->integer (-> String Integer))
(define (string->integer s)
  (let ([n (string->number s)])
    (cond
      [(exact-integer? n) n]
      [else (raise-argument-error 'string->integer "a string convertible to an exact-integer" s)])))

(check-eqv? (solve (parse-input "input.txt")) 471019)

#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         rackunit)

(define preamble-length 25)

(define (solve input-file-path)
  (let ([numbers (input->numbers input-file-path)])
    (find-invalid-number (drop numbers preamble-length) (take numbers preamble-length))))

(define (find-invalid-number numbers preamble)
  (cond
    [(null? numbers) (raise-argument-error 'find-invalid-number "non-empty list" numbers)]
    [(< (length preamble) 2) (raise-argument-error 'find-invalid-number "list of length >= 2" preamble)]
    [else (let ([num (car numbers)])
            (cond
              [(not (valid-number? preamble num)) num]
              [else (find-invalid-number (cdr numbers) (append (cdr preamble) (list num)))]))]))

(define (generate-pairs numbers [pairs '()])
  (cond
    [(<= (length numbers) 1) pairs]
    [else (let* ([head (car numbers)]
                 [tail (cdr numbers)]
                 [new-pairs (map (curry cons head) tail)])
            (generate-pairs tail (append new-pairs pairs)))]))

(define (valid-number? preamble num)
  (for/or ([pair (generate-pairs preamble)])
    (valid-pair? pair num)))

(define (valid-pair? pair sum)
  (match pair
    [(cons a a) #f]
    [(cons a b) (= (+ a b) sum)]))

(define (input->numbers input-file-path)
  (map string->number (file->lines input-file-path)))

(check-eqv? (solve "input.txt") 23278925)

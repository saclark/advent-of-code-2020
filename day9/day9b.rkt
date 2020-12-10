#lang racket/base

(require racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         threading)

(define preamble-length 25)

(define (day9b input-file-path)
  (let* ([numbers (input->numbers input-file-path)]
         [sum (find-invalid-number (drop numbers preamble-length) (take numbers preamble-length))]
         [set (find-contiguous-set-that-sums-to-n numbers sum)]
         [sorted-set (sort set <)])
    (+ (car set) (last sorted-set))))

(define (find-contiguous-set-that-sums-to-n numbers sum [n 2])
  (if (> n (length numbers))
      #f
      (let ([group (for/first ([group (list->contiguous-groups numbers n)]
                               #:when (= sum (apply + group)))
                     group)])
        (if group
            group
            (find-contiguous-set-that-sums-to-n numbers sum (+ n 1))))))

(define (list->contiguous-groups ls n [grouped '()])
  (cond
    [(< (length ls) n) (append grouped (list ls))]
    [else (list->contiguous-groups (cdr ls) n (append grouped (list (take ls n))))]))

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

(day9b "day9-input.txt")
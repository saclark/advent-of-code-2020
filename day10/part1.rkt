#lang racket/base

(require racket/file
         racket/function
         racket/list)

(define (solve ratings)
  (let* ([sorted-ratings (sort ratings <)]
         [built-in-rating (+ 3 (last sorted-ratings))]
         [all-ratings (cons 0 (append sorted-ratings (list built-in-rating)))]
         [diffs (in-differences all-ratings)])
    (* (count (curry = 1) diffs) (count (curry = 3) diffs))))

(define (in-differences numbers)
  (for/fold ([acc '()]
             [prev #f]
             #:result acc)
            ([n numbers])
    (if prev
      (values (append acc (list (- n prev))) n)
      (values acc n))))

(define (parse-input input-file-path)
  (map string->number (file->lines input-file-path)))

(solve (parse-input "input.txt"))

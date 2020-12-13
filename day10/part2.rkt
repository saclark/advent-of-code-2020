#lang racket/base

(require racket/file
         racket/list)

(define (solve ratings)
  (let* ([sorted-ratings (sort ratings <)]
         [built-in-rating (+ 3 (last sorted-ratings))]
         [all-ratings (cons 0 (append sorted-ratings (list built-in-rating)))]
         [diffs (in-differences all-ratings)])
    (apply * (map count-arrangements (group-omittable-adapters diffs)))))

(define (group-omittable-adapters diffs [prev #f] [group '()] [groups '()])
  (if (null? diffs)
      groups
      (let ([diff (car diffs)])
        (if (and prev ((+ prev diff) . <= . 3))
            (group-omittable-adapters (cdr diffs) diff (cons diff group) groups)
            (if (null? group)
                (group-omittable-adapters (cdr diffs) diff group groups)
                (group-omittable-adapters (cdr diffs) diff '() (cons group groups)))))))

(define (count-arrangements omittable-adapter-run)
  (case (length omittable-adapter-run)
    [(1) 2]
    [(2) 4]
    [(3) 7]
    [(4) 13]
    [(5) 24]
    [else (raise-argument-error 'count-arrangements "6 or fewer adapters" omittable-adapter-run)]))

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

#lang racket/base

(require racket/file
         racket/list
         racket/string
         rackunit)

(define (solve starting-numbers n)
  (define history (make-hasheq))
  (for ([(n i) (in-indexed (drop-right starting-numbers 1))])
    (hash-set! history n (add1 i)))
  (for/fold ([p (last starting-numbers)])
            ([i (in-range (add1 (length starting-numbers)) (add1 n))])
    (let ([last-seen (hash-ref history p #f)])
      (hash-set! history p (sub1 i))
      (if last-seen (- (sub1 i) last-seen) 0))))

(define (parse-input input-file-path)
  (map string->number (string-split (file->string input-file-path) ",")))

(check-eqv? (solve (parse-input "input.txt") 30000000) 1437692)

#lang racket/base

(require racket/file
         racket/match
         racket/string
         rackunit)

(define target-bag-color "shiny gold")

(define (solve input-file-path)
  (let ([bag-specs (parse-input-file input-file-path)])
    (count-inner-bags bag-specs (hash-ref bag-specs target-bag-color))))

(define (count-inner-bags bag-specs bag-contents)
  (cond
    [(hash-empty? bag-contents) 0]
    [else (for/fold ([acc 0])
                    ([kv (hash->list bag-contents)])
            (let ([color (car kv)]
                  [num (cdr kv)])
              (+ acc num (* num (count-inner-bags bag-specs (hash-ref bag-specs color))))))]))

(define (parse-input-file input-file-path)
  (for/fold ([acc (hash)])
            ([line (file->lines input-file-path)])
    (let ([bag-spec (parse-bag-spec line)])
      (hash-set acc (car bag-spec) (cdr bag-spec)))))

(define (parse-bag-spec line)
  (let* ([bag-spec-components (string-split line #rx" bags contain | bags?, | bags?\\.")]
         [bag-color (car bag-spec-components)]
         [inner-bags (parse-inner-bag-specs (cdr bag-spec-components))])
    (cons bag-color inner-bags)))

(define (parse-inner-bag-specs specs)
  (for/fold ([acc (hash)])
            ([spec specs])
    (match (parse-inner-bag-spec spec)
      ['() acc]
      [(cons color num) (hash-set acc color num)])))

(define (parse-inner-bag-spec spec)
  (cond
    [(string=? spec "no other") '()]
    [else
     (match-let ([(list num color) (car (regexp-match* #rx"([0-9]) (.+)" spec #:match-select cdr))])
       (cons color (string->number num)))]))

(check-eqv? (solve "input.txt") 13264)

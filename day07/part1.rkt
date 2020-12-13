#lang racket/base

(require racket/file
         racket/match
         racket/string)

(define target-bag-color "shiny gold")

(define (solve input-file-path)
  (let ([bag-specs (parse-input-file input-file-path)])
    (for/sum ([bag-color (hash-keys bag-specs)])
      (if (and (not (string=? bag-color target-bag-color))
               (bag-contains-color? bag-specs bag-color target-bag-color))
          1
          0))))

(define (bag-contains-color? bag-specs bag-color contains-color)
  (let ([bag-contents (hash-ref bag-specs bag-color (hash))])
    (cond
      [(string=? bag-color contains-color) #t]
      [(hash-empty? bag-contents) #f]
      [else (for/or ([color (hash-keys bag-contents)])
              (bag-contains-color? bag-specs color contains-color))])))

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

(solve "input.txt")

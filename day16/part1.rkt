#lang racket/base

(require racket/file
         racket/match
         racket/string
         rackunit)

(define (solve rules nearby-tickets)
  (define breaks-rules? (λ (n) (for/and ([rule rules]) (not (valid-number? rule n)))))
  (for/sum ([ticket nearby-tickets])
    (for/sum ([num ticket])
      (if (breaks-rules? num) num 0))))

(struct rule (name min1 max1 min2 max2) #:transparent)

(define (valid-number? rule n)
  (or (and ((rule-min1 rule) . <= . n) (n . <= . (rule-max1 rule)))
      (and ((rule-min2 rule) . <= . n) (n . <= . (rule-max2 rule)))))

(define (parse-input input-file-path)
  (match (string-split (file->string input-file-path) "\n\n")
    [(list rules-str my-ticket-str nearby-tickets-str)
      (values (parse-rules rules-str)
              (parse-ticket (cadr (string-split my-ticket-str "\n")))
              (map parse-ticket (cdr (string-split nearby-tickets-str "\n"))))]))

(define (parse-rules rules-str)
  (for/list ([line (string-split rules-str "\n")])
    (match-let ([(list name min1 max1 min2 max2)
                 (car (regexp-match* #rx"(.+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)"
                                     line
                                     #:match-select cdr))])
      (rule name
            (string->number min1)
            (string->number max1)
            (string->number min2)
            (string->number max2)))))

(define (parse-ticket ticket-str)
  (map string->number (string-split ticket-str ",")))

(let-values ([(rules _ nearby-tickets) (parse-input "input.txt")])
  (check-eqv? (solve rules nearby-tickets) 19093))
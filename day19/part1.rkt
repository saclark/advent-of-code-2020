#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         racket/string
         typed/rackunit)

(define (solve rules messages)
  (count identity (map (curry matches-rule? rules 0) messages)))

(define (matches-rule? rules rule-number message)
  (match-let ([(cons partial-match? unmatched-chars)
               (partially-matches-rule? rules rule-number (string->list message))])
    (and partial-match? (null? unmatched-chars))))

(define (partially-matches-rule? rules v message)
  (cond
    [(null? message)
      (cons #f '())]
    [(char? v)
      (if (eqv? v (car message))
          (cons #t (cdr message))
          (cons #f message))]
    [(exact-integer? v)
      (let* ([rule (vector-ref rules v)])
        (or (for/or ([option (in-list rule)])
              (for/fold ([result (cons #t message)]
                         #:result (if (car result) result #f))
                        ([v (in-list option)])
                (if (car result)
                    (partially-matches-rule? rules v (cdr result))
                    (cons #f message))))
            (cons #f message)))]))

(define (parse-input input-file-path)
  (match-let ([(list rules-str messges-str) (string-split (file->string input-file-path) "\n\n")])
    (values (parse-rules rules-str) (string-split messges-str "\n"))))

(define (parse-rules rules-str)
  (let* ([rules (map parse-rule-line (string-split rules-str "\n"))]
         [rule-tree (make-vector (add1 (apply max (map car rules))))])
    (for ([rule (in-list rules)])
      (vector-set! rule-tree (car rule) (cdr rule)))
    rule-tree))

(define (parse-rule-line line)
  (match-let* ([(list id spec) (string-split line ": ")])
    (cons (string->integer id) (parse-rule-spec spec))))

(define (parse-rule-spec spec)
  (define parse-rule-option (λ (s) (map string->integer (string-split s " "))))
  (match (string-split spec " | ")
    [(list "\"a\"") (list (list #\a))]
    [(list "\"b\"") (list (list #\b))]
    [(list rule-options ...) (map parse-rule-option rule-options)]))

(define (string->integer s)
  (let ([n (string->number s)])
    (cond
      [(exact-integer? n) n]
      [else (raise-argument-error 'string->integer "a string convertible to an exact-integer" s)])))

(let-values ([(rules messages) (parse-input "input.txt")])
  (check-eqv? (solve rules messages) 241))

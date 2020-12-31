#lang typed/racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         racket/string
         typed/rackunit)

(define-type Rule (Listof Rule-Option))
(define-type Rule-Option (Listof (U Integer Char)))
(define-type Message (Listof Char))

(: solve (-> (Vectorof Rule) (Listof String) Integer))
(define (solve rules messages)
  (: matches-rule-zero? (-> String Boolean))
  (define matches-rule-zero? (λ (msg) (matches-rule? rules 0 msg)))
  (count identity (map matches-rule-zero? messages)))

(: matches-rule? (-> (Vectorof Rule) Integer String Boolean))
(define (matches-rule? rules rule-number message)
  (match-let ([(cons partial-match? unmatched-chars)
               (partially-matches-rule? rules rule-number (string->list message))])
    (and partial-match? (null? unmatched-chars))))

(: partially-matches-rule? (-> (Vectorof Rule) (U Integer Char) Message (Pairof Boolean Message)))  
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
        (or (for/or : (U (Pairof Boolean Message) False) ([option (in-list rule)])
              (for/fold ([result : (Pairof Boolean Message) (cons #t message)]
                         #:result (if (car result) result #f))
                        ([v (in-list option)])
                (if (car result)
                    (partially-matches-rule? rules v (cdr result))
                    (cons #f message))))
            (cons #f message)))]))

(: parse-input (-> String (Values (Vectorof Rule) (Listof String))))
(define (parse-input input-file-path)
  (match-let ([(list rules-str messges-str) (string-split (file->string input-file-path) "\n\n")])
    (values (parse-rules rules-str) (string-split messges-str "\n"))))

(: parse-rules (-> String (Vectorof Rule)))
(define (parse-rules rules-str)
  (: make-rule-tree (-> (Listof (Pairof Integer Rule)) (Vectorof Rule)))
  (define make-rule-tree
    (λ (rules)
      (let ([len (add1 (apply max (map (ann car (-> (Pairof Integer Rule) Integer)) rules)))])
        (make-vector len '(())))))
  (let* ([rules (map parse-rule-line (string-split rules-str "\n"))]
         [rule-tree (make-rule-tree rules)])
    (for ([rule (in-list rules)])
      (vector-set! rule-tree (car rule) (cdr rule)))
    rule-tree))

(: parse-rule-line (-> String (Pairof Integer Rule)))
(define (parse-rule-line line)
  (match-let* ([(list id spec) (string-split line ": ")])
    (cons (string->integer id) (parse-rule-spec spec))))

(: parse-rule-spec (-> String Rule))
(define (parse-rule-spec spec)
  (: parse-rule-option (-> String (Listof Integer)))
  (define parse-rule-option (λ (s) (map string->integer (string-split s " "))))
  (match (string-split spec " | ")
    [(list "\"a\"") (list (list #\a))]
    [(list "\"b\"") (list (list #\b))]
    [(list rule-options ...) (map parse-rule-option rule-options)]))

(: string->integer (-> String Integer))
(define (string->integer s)
  (let ([n (string->number s)])
    (cond
      [(exact-integer? n) n]
      [else (raise-argument-error 'string->integer "a string convertible to an exact-integer" s)])))

(let-values ([(rules messages) (parse-input "input.txt")])
  (check-eqv? (solve rules messages) 241))

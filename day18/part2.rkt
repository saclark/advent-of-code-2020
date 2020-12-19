#lang racket/base

(require racket/file
         racket/match
         rackunit)

(define (solve exprs)
  (apply + (map eval exprs)))

(define (right l r) r)

(define (eval expr [op right] [result #f])
  (if (null? expr)
    result
    (let ([v (car expr)])
      (cond
        [(list? v)   (eval (cdr expr) right (op result (eval v)))]
        [(number? v) (eval (cdr expr) right (op result v))]
        [(eqv? '* v) (* result (eval (cdr expr)))]
        [(eqv? '+ v) (eval (cdr expr) + result)]
        [else (raise-argument-error 'eval "list?, number?, '*, or '+" v)]))))

(define (parse-input input-file-path)
  (map (compose cdr parse-expr string->list) (file->lines input-file-path)))

(define (parse-expr chars [expr '()])
  (if (null? chars)
    (cons chars (reverse expr))
    (let ([c (car chars)])
      (cond
        [(eqv? #\) c) (cons (cdr chars) (reverse expr))]
        [(eqv? #\( c) (match-let ([(cons chars subexpr) (parse-expr (cdr chars))])
                        (parse-expr chars (cons subexpr expr)))]
        [(eqv? #\* c) (parse-expr (cdr chars) (cons '* expr))]
        [(eqv? #\+ c) (parse-expr (cdr chars) (cons '+ expr))]
        [(char-numeric? c) (match-let ([(cons chars num) (parse-number chars)])
                             (parse-expr chars (cons num expr)))]
        [(char-whitespace? c) (parse-expr (cdr chars) expr)]
        [else (raise-argument-error 'parse-expr "number, +, *, (, or )" 'chars)]))))

(define (parse-number chars [digits '()])
  (if (or (null? chars) (not (char-numeric? (car chars))))
    (cons chars (string->number (list->string (reverse digits))))
    (parse-number (cdr chars) (cons (car chars) digits))))

(check-eqv? (solve (parse-input "input.txt")) 141993988282687)

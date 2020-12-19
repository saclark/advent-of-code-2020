#lang racket/base

(require racket/file
         racket/match
         rackunit)

(define (solve exprs)
  (apply + (map (compose car eval) exprs)))

(define (right l r) r)

(define (eval expr [op right] [result #f])
  (if (null? expr)
    (cons result expr)
    (let ([c (car expr)])
      (cond
        [(eqv? #\) c) (cons result (cdr expr))]
        [(eqv? #\( c) (match-let ([(cons num expr-rest) (eval (cdr expr))])
                        (eval expr-rest right (op result num)))]
        [(char-numeric? c) (match-let ([(cons num expr-rest) (parse-number expr)])
                            (eval expr-rest right (op result num)))]
        [(eqv? #\* c) (eval (cdr expr) * result)]
        [(eqv? #\+ c) (eval (cdr expr) + result)]
        [(char-whitespace? c) (eval (cdr expr) op result)]
        [else (raise-argument-error 'eval "+, *, (, ), a number, or whitespace" 'expr)]))))

(define (parse-number expr [digits '()])
  (if (or (null? expr)
          (not (char-numeric? (car expr))))
    (cons (string->number (list->string (reverse digits))) expr)
    (parse-number (cdr expr) (cons (car expr) digits))))

(define (parse-input input-file-path)
  (map string->list (file->lines input-file-path)))

(check-eqv? (solve (parse-input "input.txt")) 53660285675207)

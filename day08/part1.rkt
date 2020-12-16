#lang racket/base

(require racket/file
         racket/match
         racket/set
         racket/string
         rackunit
         (prefix-in srfi13: srfi/13))

(define (solve input-file-path)
  (excute-instructions (parse-instructions input-file-path)))

(define (excute-instructions instructions [i 0] [acc 0] [visited (set)])
  (cond
    [(or (< i 0) (>= i (length instructions))) (raise-argument-error 'execute-instructions "index out of bouds" i)]
    [(set-member? visited i) acc]
    [else (match-let ([(cons op n) (list-ref instructions i)]
                      [v (set-add visited i)])
            (case op
                [("nop") (excute-instructions instructions (+ i 1) acc v)]
                [("acc") (excute-instructions instructions (+ i 1) (+ acc n) v)]
                [("jmp") (excute-instructions instructions (+ i n) acc v)]))]))

(define (parse-instructions instructions-file)
  (for/list ([line (file->lines instructions-file)])
    (match-let ([(list op arg) (string-split line)])
      (cons op (parse-arg arg)))))

(define (parse-arg arg)
  (let ([sign (srfi13:string-take arg 1)]
        [num (string->number (srfi13:string-drop arg 1))])
    (match sign
      ["-" (- num)]
      ["+" num])))

(check-eqv? (solve "input.txt") 1723)

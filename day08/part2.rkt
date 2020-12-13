#lang racket

(require racket/file
         racket/format
         srfi/13)

(define (solve input-file-path)
  (excute-instructions (parse-instructions input-file-path)))

; todo: This is ugly and in-elegant. Clean it up/find better way of expressing this.
(define (excute-instructions instructions [i 0] [acc 0] [visited (set)] [flipped (set)] [flip? #t] [pre-flip-state (list 0 0 (set))])
  (cond
    [(< i 0) (raise-argument-error 'execute-instructions "index out of bouds" i)]
    [(= i (length instructions)) acc]
    [(or (set-member? visited i) (> i (length instructions)))
      (excute-instructions instructions
                           (car pre-flip-state)
                           (cadr pre-flip-state)
                           (caddr pre-flip-state)
                           flipped
                           #t
                           pre-flip-state)]
    [else (match-let ([(cons opcode n) (list-ref instructions i)]
                      [v (set-add visited i)])
            (case opcode
              [("acc") (excute-instructions instructions (+ i 1) (+ acc n) v flipped flip? pre-flip-state)]
              [("nop") (if (or (not flip?) (set-member? flipped i))
                           (excute-instructions instructions (+ i 1) acc v flipped flip? pre-flip-state)
                           (excute-instructions instructions (+ i n) acc v (set-add flipped i) #f (list i acc visited)))]
              [("jmp") (if (or (not flip?) (set-member? flipped i))
                           (excute-instructions instructions (+ i n) acc v flipped flip? pre-flip-state)
                           (excute-instructions instructions (+ i 1) acc v (set-add flipped i) #f (list i acc visited)))]))]))

(define (parse-instructions instructions-file)
  (for/list ([line (file->lines instructions-file)])
    (match-let ([(list opcode arg) (string-split line)])
      (cons opcode (parse-arg arg)))))

(define (parse-arg arg)
  (let ([sign (string-take arg 1)]
        [num (string->number (string-drop arg 1))])
    (match sign
      ["-" (- num)]
      ["+" num])))

(solve "input.txt")

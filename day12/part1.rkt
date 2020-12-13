#lang racket/base

(require racket/file
         racket/match)

(define (solve instructions)
  (define exec (λ (instruction s) (execute-instruction instruction s)))
  (manhattan-distance (foldl exec (state 0 0 0) instructions)))

(define (manhattan-distance state)
  (+ (abs (state-x state)) (abs (state-y state))))

(struct state (x y m))

(define (move s x+ y+)
  (state (+ x+ (state-x s))
         (+ y+ (state-y s))
         (state-m s)))

(define (rotate s degrees)
  (state (state-x s)
         (state-y s)
         (modulo (+ degrees (state-m s)) 360)))

(define (move-forward s n)
  (case (state-m s)
    [(0)   (move s n 0)]
    [(90)  (move s 0 n)]
    [(180) (move s (- n) 0)]
    [(270) (move s 0 (- n))]
    [else (raise-argument-error 'move-forward "one of: 0, 90, 180, 270" n)]))

(define (execute-instruction instruction state)
  (match instruction
    [(cons 'N n) (move state 0 n)]
    [(cons 'S n) (move state 0 (- n))]
    [(cons 'E n) (move state n 0)]
    [(cons 'W n) (move state (- n) 0)]
    [(cons 'L n) (rotate state n)]
    [(cons 'R n) (rotate state (- n))]
    [(cons 'F n) (move-forward state n)]))

(define (parse-input input-file-path)
  (define list->number (λ (digits) (string->number (list->string digits))))
  (for/list ([chars (map string->list (file->lines input-file-path))])
    (match-let ([(list char digits ...) chars])
      (cons (string->symbol (string char))
            (list->number digits)))))

(solve (parse-input "input.txt"))

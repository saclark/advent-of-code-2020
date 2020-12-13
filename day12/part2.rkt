#lang racket

(require racket/file
         racket/match)

(define (solve instructions)
  (define exec (λ (instruction s) (execute-instruction instruction s)))
  (manhattan-distance (foldl exec (state 0 0 10 1) instructions)))

(define (manhattan-distance state)
  (+ (abs (state-x state)) (abs (state-y state))))

(struct state (x y x-offset y-offset))

(define (set-offsets s x-offset y-offset)
  (state (state-x s) (state-y s) x-offset y-offset))

(define (forward s n)
  (move-ship s (* n (state-x-offset s)) (* n (state-y-offset s))))

(define (move-ship s x+ y+)
  (state (+ x+ (state-x s))
         (+ y+ (state-y s))
         (state-x-offset s)
         (state-y-offset s)))

(define (move-waypoint s x-offset+ y-offset+)
  (state (state-x s)
         (state-y s)
         (+ x-offset+ (state-x-offset s))
         (+ y-offset+ (state-y-offset s))))

(define (rotate-waypoint s degrees)
  (case (modulo degrees 360)
    [(0)   s]
    [(90)  (set-offsets s (- (state-y-offset s)) (state-x-offset s))]
    [(180) (set-offsets s (- (state-x-offset s)) (- (state-y-offset s)))]
    [(270) (set-offsets s (state-y-offset s) (- (state-x-offset s)))]
    [else (raise-argument-error 'rotate-waypoint "0 or a value divisible by 90" degrees)]))

(define (execute-instruction instruction state)
  (match instruction
    [(cons 'N n) (move-waypoint state 0 n)]
    [(cons 'S n) (move-waypoint state 0 (- n))]
    [(cons 'E n) (move-waypoint state n 0)]
    [(cons 'W n) (move-waypoint state (- n) 0)]
    [(cons 'L n) (rotate-waypoint state n)]
    [(cons 'R n) (rotate-waypoint state (- n))]
    [(cons 'F n) (forward state n)]))

(define (parse-input input-file-path)
  (define list->number (λ (digits) (string->number (list->string digits))))
  (for/list ([chars (map string->list (file->lines input-file-path))])
    (match chars
      [(list char digits ...) (cons (string->symbol (string char))
                                    (list->number digits))])))

(solve (parse-input "input.txt"))

#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         rackunit
         threading
         "cantor.rkt")

(define (solve space cycles)
  (let ([end-space (for/fold ([s space])
                             ([_ (in-range cycles)])
                     (run-iteration s))])
  (count identity (hash-values (space-points end-space)))))

(define (run-iteration space)
  (expand-space! space)
  (define new-space (make-new-space space))
  (for ([z (in-range (space-min-z space) (add1 (space-max-z space)))])
    (for ([y (in-range (space-min-y space) (add1 (space-max-y space)))])
      (for ([x (in-range (space-min-x space) (add1 (space-max-x space)))])
        (let* ([p (point x y z)]
               [active? (next-state space p)])
          (when active? (set-space-point! new-space p #t))))))
  new-space)

(struct space (points min-x min-y min-z max-x max-y max-z) #:mutable)

(define (make-new-space s)
  (space (make-hasheq)
         (space-min-x s)
         (space-min-y s)
         (space-min-z s)
         (space-max-x s)
         (space-max-y s)
         (space-max-z s)))

(define (expand-space! space)
  (set-space-min-x! space (sub1 (space-min-x space)))
  (set-space-min-y! space (sub1 (space-min-y space)))
  (set-space-min-z! space (sub1 (space-min-z space)))
  (set-space-max-x! space (add1 (space-max-x space)))
  (set-space-max-y! space (add1 (space-max-y space)))
  (set-space-max-z! space (add1 (space-max-z space))))

(define (set-space-point! space point v)
  (hash-set! (space-points space) (point-key point) v))

(struct point (x y z) #:transparent)

(define (point-key point)
  (signed-numbers->cantor (point-x point) (point-y point) (point-z point)))

(define (next-state space point)
  (let ([n (active-neighbor-count space point)])
    (if (point-active? space point)
      (if (or (= n 2) (= n 3)) #t #f)
      (if (not (= n 3)) #f #t))))

(define (active-neighbor-count space point)
  (count identity (surrounding-states space point)))

(define (surrounding-states space point)
    (for/fold ([acc '()])
              ([x+ '(-1 0 1)])
      (for/fold ([acc2 acc])
                ([y+ '(-1 0 1)])
        (for/fold ([acc3 acc2])
                  ([z+ '(-1 0 1)])
          (if (and (= x+ 0) (= y+ 0) (= z+ 0))
              acc3
              (cons (point-active? space (neighbor point x+ y+ z+)) acc3))))))

(define (neighbor pnt x+ y+ z+)
  (point (+ (point-x pnt) x+)
         (+ (point-y pnt) y+)
         (+ (point-z pnt) z+)))

(define (point-active? space point)
  (hash-ref (space-points space) (point-key point) #f))

(define (parse-input input-file-path)
  (define chars->states (λ (chars) (map char->state chars)))
  (define char->state (λ (char) (match char [#\. #f] [#\# #t])))
  (~> input-file-path
      file->lines
      (map string->list _)
      (map chars->states _)
      layer->space))

(define (layer->space layer)
  (let* ([h (sub1 (length layer))]
         [w (sub1 (apply max (map length layer)))]
         [pocket-space (space (make-hasheq) 0 0 0 w h 0)])
    (for ([(row y) (in-indexed layer)])
      (for ([(active? x) (in-indexed row)])
        (set-space-point! pocket-space (point x y 0) active?)))
     pocket-space))

(check-eqv? (solve (parse-input "input.txt") 6) 362)

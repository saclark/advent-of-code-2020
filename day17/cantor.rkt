#lang racket/base

(require rackunit)

(provide numbers->cantor
         cantor->numbers
         signed-numbers->cantor
         cantor->signed-numbers)

(define (numbers->cantor . nums)
  (foldl (λ (y x) (cantor-pair x y)) (car nums) (cdr nums)))

(define (cantor->numbers cantor [n 2])
  (define cantor->numbers*
    (λ (cantor n nums)
      (if (= 1 n)
          (cons cantor nums)
          (let-values ([(x y) (reverse-cantor-pair cantor)])
            (cantor->numbers* x (- n 1) (cons y nums))))))
  (cantor->numbers* cantor n '()))

(define (cantor-pair x y)
  (+ (/ (* (+ x y) (+ x y 1)) 2) y))

(define (reverse-cantor-pair cantor)
  (let* ([t (inexact->exact (floor (/ (- (sqrt (+ 1 (* 8 cantor))) 1) 2)))]
         [x (- (* t (/ (+ t 3) 2)) cantor)]
         [y (- cantor (* t (/ (+ t 1) 2)))])
    (values x y)))

(define (signed-numbers->cantor . nums)
  (foldl (λ (y x) (signed-cantor-pair x y)) (car nums) (cdr nums)))

(define (cantor->signed-numbers cantor [n 2])
  (define cantor->signed-numbers*
    (λ (cantor n nums)
      (if (= 0 n)
          nums
          (let-values ([(x y) (reverse-signed-cantor-pair cantor)])
            (cantor->signed-numbers* x (- n 1) (cons y nums))))))
  (cantor->signed-numbers* cantor n '()))

(define (signed-cantor-pair x y)
  (cantor-pair (if (>= x 0) (* 2 x) (+ (* -2 x) 1))
               (if (>= y 0) (* 2 y) (+ (* -2 y) 1))))

(define (reverse-signed-cantor-pair cantor)
  (let*-values ([(x* y*) (reverse-cantor-pair cantor)]
                [(x) (if (= 0 (modulo x* 2)) (/ x* 2) (/ (- 1 x*) 2))]
                [(y) (if (= 0 (modulo y* 2)) (/ y* 2) (/ (- 1 y*) 2))])
    (values x y)))

(let ([unsigned '(0 1 2 3 4 5)]
      [signed '(0 -1 2 -3 4 -5)])
  (check-equal? (cantor->numbers (apply numbers->cantor unsigned) (length unsigned)) unsigned)
  (check-equal? (cantor->signed-numbers (apply signed-numbers->cantor signed) (length signed)) signed))

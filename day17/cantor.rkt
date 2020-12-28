#lang racket/base

(provide numbers->cantor
         cantor->numbers
         signed-numbers->cantor
         cantor->signed-numbers)

;; todo: define contracts to enforce constraints around function arguments

(define (numbers->cantor . nums)
  (foldl (λ (y x) (cantor-pair x y)) (car nums) (cdr nums)))

(define (cantor->numbers cantor num-of-constituents)
  (define cantor->numbers-aux
    (λ (cantor num-of-constituents nums)
      (unless (positive? num-of-constituents)
              (raise-argument-error 'cantor->numbers "positive?" num-of-constituents))
      (if (= 1 num-of-constituents)
          (cons cantor nums)
          (let-values ([(x y) (reverse-cantor-pair cantor)])
            (cantor->numbers-aux x (- num-of-constituents 1) (cons y nums))))))
  (cantor->numbers-aux cantor num-of-constituents '()))

(define (cantor-pair x y)
  (+ (/ (* (+ x y) (+ x y 1)) 2) y))

(define (reverse-cantor-pair cantor)
  (let* ([t (inexact->exact (floor (/ (- (sqrt (+ 1 (* 8 cantor))) 1) 2)))]
         [x (- (* t (/ (+ t 3) 2)) cantor)]
         [y (- cantor (* t (/ (+ t 1) 2)))])
    (values x y)))

(define (signed-numbers->cantor . nums)
  (foldl (λ (y x) (signed-cantor-pair x y)) (car nums) (cdr nums)))

(define (cantor->signed-numbers cantor num-of-constituents)
  (define cantor->signed-numbers-aux
    (λ (cantor num-of-constituents nums)
      (unless (positive? num-of-constituents)
              (raise-argument-error 'cantor->signed-numbers "positive?" num-of-constituents))
      (if (= 1 num-of-constituents)
          (cons cantor nums)
          (let-values ([(x y) (reverse-signed-cantor-pair cantor)])
            (cantor->signed-numbers-aux x (- num-of-constituents 1) (cons y nums))))))
  (cantor->signed-numbers-aux cantor num-of-constituents '()))

(define (signed-cantor-pair x y)
  (cantor-pair (if (>= x 0) (* 2 x) (+ (* -2 x) 1))
               (if (>= y 0) (* 2 y) (+ (* -2 y) 1))))

(define (reverse-signed-cantor-pair cantor)
  (let*-values ([(x* y*) (reverse-cantor-pair cantor)]
                [(x) (if (= 0 (modulo x* 2)) (/ x* 2) (/ (- 1 x*) 2))]
                [(y) (if (= 0 (modulo y* 2)) (/ y* 2) (/ (- 1 y*) 2))])
    (values x y)))


;;(require rackunit)
;;(let ([unsigned '(0 1 2 3 4 5)]
;;      [signed '(0 -1 2 -3 4 -5)])
;;  (check-equal? (cantor->numbers (apply numbers->cantor unsigned) (length unsigned)) unsigned)
;;  (check-equal? (cantor->signed-numbers (apply signed-numbers->cantor signed) (length signed)) signed))

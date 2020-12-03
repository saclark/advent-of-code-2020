#lang racket

(define b 0)

(define (day3b grid slopes)
  (for/product ([m slopes])
    (for/sum ([y (range (length grid))])
        (let ([row (list-ref grid y)]
              [x (get-x-coord (- y) m b)])
          (if (and (whole-num? x)
                   (list-ref row (modulo x (length row))))
              1
              0)))))

(define (whole-num? x)
  (= x (floor x)))

(define (get-x-coord y m b)
  (/ (- y b) m))

(define (get-input file-path)
  (for/list ([line (file->lines file-path)])
    (for/list ([char (string->list line)])
      (eq? char #\#))))

(day3b (get-input "day3-input.txt")
       (list (- (/ 1 1))
             (- (/ 1 3))
             (- (/ 1 5))
             (- (/ 1 7))
             (- (/ 2 1))))

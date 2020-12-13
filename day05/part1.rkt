#lang racket/base

(require racket/list
         racket/file
         racket/function)

(define rows (range 0 128))
(define columns (range 0 8))

(define (solve input)
  (apply max (map (compose1 seat-id decode-boarding-pass) (file->lines input))))

(define (seat-id row-col-pair)
  (+ (* (car row-col-pair) 8) (cadr row-col-pair)))

(define (decode-boarding-pass pass-string)
  (let* ([pass-chars (string->list pass-string)]
         [row-instructions (map (curry eq? #\F) (take pass-chars 7))]
         [col-instructions (map (curry eq? #\L) (drop pass-chars 7))])
    (list (binary-find rows row-instructions)
          (binary-find columns col-instructions))))

(define (binary-find ls instructions)
  (let ([result (binary-filter ls instructions)])
    (if (= 1 (length result))
        (car result)
        (raise-argument-error 'binary-find "list with length of 1" ls))))

(define (binary-filter ls instructions)
  (let ([ls-length (length ls)])
    (cond
      [(or (<= ls-length 1)
           (zero? (length instructions))) ls]
      [else (if (car instructions)
                (binary-filter (take ls (/ ls-length 2)) (cdr instructions))
                (binary-filter (drop ls (/ ls-length 2)) (cdr instructions)))])))

(solve "input.txt")

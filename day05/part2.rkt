#lang racket/base

(require racket/list
         racket/file
         racket/function
         racket/set)

(define rows (range 0 128))
(define columns (range 0 8))

(define (solve input)
  (find-my-seat (find-missing-ids (range 0 1024) (parse-seat-ids input))))

(define (find-my-seat ids)
  (define (find-my-seat* sorted-ids [prev-two '()])
    (if (< (length prev-two) 2)
        (find-my-seat* (cdr sorted-ids) (cons (car sorted-ids) prev-two))
        (let ([id (car sorted-ids)]
              [prev-id (car prev-two)]
              [prev-prev-id (cadr prev-two)])
          (if (and (> (- prev-id prev-prev-id) 1)
                   (> (- id prev-id) 1))
              prev-id
              (find-my-seat* (cdr sorted-ids) (list id prev-id))))))
  (if (< (length ids) 3)
      (raise-argument-error 'find-my-seat "list of with length >= 3" ids)
      (find-my-seat* (sort ids <))))

(define (find-missing-ids exp-ids act-ids)
  (let ([act-id-set (list->set act-ids)])
    (for/fold ([missing-ids '()]
               #:result (reverse missing-ids))
              ([id exp-ids])
      (if (set-member? act-id-set id)
          missing-ids
          (cons id missing-ids)))))

(define (parse-seat-ids input)
  (map (compose1 seat-id decode-boarding-pass) (file->lines input)))

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

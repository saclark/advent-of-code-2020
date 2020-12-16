#lang racket/base

(require racket/file
         racket/match
         rackunit)

(define (parse-record record)
  (match-let ([(list min max char password)
               (car (regexp-match* #rx"([0-9]+)-([0-9]+) ([a-zA-Z]): ([a-zA-Z]+)"
                                   record
                                   #:match-select cdr))])
    (list (string->number min)
          (string->number max)
          (car (string->list char))
          password)))

(define (valid-password? min max char password)
  (let ([count (for/sum ([c password]) (if (eqv? c char) 1 0))])
    (if (or (< count min) (> count max)) #f #t)))

(define (valid-record? record)
  (apply valid-password? (parse-record record)))

;; Synchronous
(define (solve records)
  (for/sum ([record records])
    (if (valid-record? record) 1 0)))

;; Asynchronous
(define (solve-async records)
  (let ([counter (current-thread)])
    (for ([record records])
      (thread (lambda ()
                (thread-send counter (valid-record? record)))))
    (for/sum ([i (length records)])
      (if (thread-receive) 1 0))))

(check-eqv? (solve (file->lines "input.txt")) 548)

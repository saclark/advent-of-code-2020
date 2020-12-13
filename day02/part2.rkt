#lang racket/base

(require racket/file
         racket/match)

(define (solve records)
  (for/sum ([record records])
    (if (apply valid-password? (parse-record record)) 1 0)))

(define (parse-record record)
  (match-let ([(list pos1 pos2 char password)
               (car (regexp-match* #rx"([0-9]+)-([0-9]+) ([a-zA-Z]): ([a-zA-Z]+)"
                                   record
                                   #:match-select cdr))])
    (list (string->number pos1)
          (string->number pos2)
          (car (string->list char))
          password)))

(define (valid-password? pos1 pos2 char password)
  (let ([pos1-char (if (>= (string-length password) pos1) (string-ref password (- pos1 1)) #f)]
        [pos2-char (if (>= (string-length password) pos2) (string-ref password (- pos2 1)) #f)])
    (if (and (or (eq? pos1-char char) (eq? pos2-char char))
             (not (eq? pos1-char pos2-char)))
        #t
        #f)))

(solve (file->lines "input.txt"))

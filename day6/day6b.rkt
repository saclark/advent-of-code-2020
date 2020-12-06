#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/string
         threading)

(define (day6a input-file)
  (for/sum ([group (~> input-file
                       file->string
                       (string-split "\n\n"))])
    (~> group
        (string-split "\n")
        count-unanimous-group-answers)))

(define (count-unanimous-group-answers people)
  (~> people
      count-group-answers
      hash-values
      (filter (curry eq? (length people)) _)
      length))

(define (count-group-answers people)
  (for/fold ([acc (hash)])
            ([person people])
    (for/fold ([acc* acc])
              ([answer (string->list person)])
      (hash-update acc* answer (lambda (v) (+ v 1)) 0))))

(day6a "day6-input.txt")

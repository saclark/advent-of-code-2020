#lang racket/base

(require racket/file
         racket/list
         racket/string
         threading)

(define (day6a input-file)
  (for/sum ([group (~> input-file file->string (string-split "\n\n"))])
    (~> group
        (string-split "\n")
        (map string->list _)
        flatten
        remove-duplicates
        length)))

(day6a "day6-input.txt")

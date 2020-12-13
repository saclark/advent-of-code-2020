#lang racket/base

(require racket/file
         racket/list
         racket/string
         threading)

(define (solve input-file)
  (for/sum ([group (~> input-file file->string (string-split "\n\n"))])
    (~> group
        (string-split "\n")
        (map string->list _)
        flatten
        remove-duplicates
        length)))

(solve "input.txt")

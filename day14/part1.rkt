#lang racket/base

(require racket/file
         racket/format
         racket/match
         racket/string)

(define int-size 36)

(struct state ([bitmask #:mutable] [memory #:mutable]))

(define program-state (state #f (make-hasheq)))

(define (solve instructions)
  (for ([instruction instructions])
    (exec-instruction program-state instruction))
  (apply + (hash-values (state-memory program-state))))

(define (exec-instruction s instruction)
  (match instruction
    [(list 'mask mask) (set-state-bitmask! s mask)]
    [(list 'mem i n) (put-state-memory! s i n)]))

(define (put-state-memory! s i v)
  (hash-set! (state-memory s) i (apply-mask (state-bitmask s) v)))

(define (apply-mask mask n)
  (let ([binary-digits (string->vector (number->binary-string n))])
    (for ([i (in-range (vector-length mask))])
      (match (vector-ref mask i)
        [#\0 (vector-set! binary-digits i #\0)]
        [#\1 (vector-set! binary-digits i #\1)]
        [else (void)]))
    (binary-string->number (vector->string binary-digits))))

(define (number->binary-string n)
  (~a (number->string n 2) #:width int-size #:pad-string "0" #:align 'right))

(define (binary-string->number s)
  (string->number s 2))

(define (string->vector s)
  (list->vector (string->list s)))

(define (vector->string vec)
  (list->string (vector->list vec)))

(define (parse-input input-file-path)
  (for/list ([line (file->lines input-file-path)])
    (match (string-split line #rx" = |\\[|\\]")
      [(list "mask" mask) (list 'mask (string->vector mask))]
      [(list "mem" i _ n) (list 'mem (string->number i) (string->number n))])))

(solve (parse-input "input.txt"))

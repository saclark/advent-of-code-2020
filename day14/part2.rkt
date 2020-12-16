#lang racket/base

(require racket/file
         racket/format
         racket/match
         racket/string
         racket/vector
         rackunit)

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
  (for ([address (apply-mask (state-bitmask s) i)])
    (hash-set! (state-memory s) address v)))

(define (apply-mask mask address)
  (let ([address-bits (string->vector (number->binary-string address))])
    (for ([i (in-range (vector-length mask))])
      (match (vector-ref mask i)
        [#\1 (vector-set! address-bits i #\1)]
        [#\X (vector-set! address-bits i #\X)]
        [else (void)]))
    (map (λ (address) (binary-string->number (vector->string address)))
         (generate-addresses address-bits))))

(define (generate-addresses floating-address [addresses '()] [i 0])
  (cond
    [(>= i (vector-length floating-address)) (cons floating-address addresses)]
    [else (case (vector-ref floating-address i)
            [(#\X) (append addresses
                           (generate-addresses (vector-set floating-address i #\0) addresses (+ i 1))
                           (generate-addresses (vector-set floating-address i #\1) addresses (+ i 1)))]
            [else (generate-addresses floating-address addresses (+ i 1))])]))

(define (vector-set vec i v)
  (let ([copy (vector-copy vec)])
    (vector-set! copy i v)
    copy))

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

(check-eqv? (solve (parse-input "input.txt")) 3434009980379)

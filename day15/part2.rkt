#lang typed/racket/base

(require racket/file
         racket/list
         racket/string
         typed/rackunit)

(: solve (-> (Listof Integer) Integer Integer))
(define (solve starting-numbers num)
  (: history (HashTable Integer Integer))
  (define history (make-hasheq))
  (for ([(num turn) (in-indexed (drop-right starting-numbers 1))])
    (hash-set! history num (add1 turn)))
  (for/fold ([prev (last starting-numbers)])
            ([turn (in-range (add1 (length starting-numbers)) (add1 num))])
    (let ([last-seen (hash-ref history prev #f)])
      (hash-set! history prev (sub1 turn))
      (if last-seen (- (sub1 turn) last-seen) 0))))

(: parse-input (-> String (Listof Integer)))
(define (parse-input input-file-path)
  (map string->integer (string-split (file->string input-file-path) ",")))

(: string->integer (-> String Integer))
(define (string->integer s)
  (let ([n (string->number s)])
    (cond
      [(exact-integer? n) n]
      [else (raise-argument-error 'string->integer "a string convertible to an exact-integer" s)])))

(check-eqv? (solve (parse-input "input.txt") 30000000) 1437692)

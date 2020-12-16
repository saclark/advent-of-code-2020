#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/string)

(define (solve starting-numbers n)
  (play-game (make-history starting-numbers)
             (vector-ref starting-numbers (- (vector-length starting-numbers) 1))
             (+ 1 (vector-length starting-numbers))
             n))

(define (make-history starting-numbers)
  (for/fold ([history (hasheq)])
            ([i (in-range (vector-length starting-numbers))])
    (hash-set history (vector-ref starting-numbers i) (list (+ i 1)))))

(define (play-game game-history prev-num starting-turn ending-turn)
  (for/fold ([history game-history]
             [p prev-num]
             #:result p)
            ([i (range starting-turn (+ 1 ending-turn))])
      (let*-values ([(n n-history) (next history i p)])
        (values (hash-set history n n-history) n))))

(define (next history i p)
  (let ([n (match (hash-ref history p)
             [(list pi) 0]
             [(list pi ppi) (- pi ppi)])])
    (match (hash-ref history n '())
      ['() (values n (list i))]
      [(list pi _ ...) (values n (list i pi))])))

(define (parse-input input-file-path)
  (list->vector (map string->number (string-split (file->string input-file-path) ","))))

(solve (parse-input "input.txt") 2020)

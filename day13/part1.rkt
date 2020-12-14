#lang racket/base

(require racket/file
         racket/function
         racket/match
         racket/string)

(define (solve timestamp ids)
  (let ([earliest-bus (earliest-bus timestamp ids)])
    (* (bus-id earliest-bus) (bus-wait-time earliest-bus))))
  
(struct bus (id wait-time))

(define (make-bus timestamp id)
  (let ([arrival (* id (ceiling (/ timestamp id)))])
    (bus id (- arrival timestamp))))

(define (earliest-bus timestamp ids)
  (define min (λ (bus prev) (if ((bus-wait-time bus) . < . (bus-wait-time prev)) bus prev)))
  (let* ([buses (map (curry make-bus timestamp) ids)])
    (foldl min (car buses) (cdr buses))))

(define (parse-input input-file-path)
  (define bus-id? (λ (id) (not (string=? "x" id))))
  (match-let ([(list timestamp ids) (file->lines input-file-path)])
    (values (string->number timestamp)
            (map string->number (filter bus-id? (string-split ids ","))))))

(let-values ([(timestamp ids) (parse-input "input.txt")])
  (solve timestamp ids))
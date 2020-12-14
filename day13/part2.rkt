#lang racket/base

(require racket/file
         racket/string)

(define (solve buses [prev-buses '()] [t 0] [inc 1])
  (if (null? buses)
    t
    (let* ([bus (car buses)]
           [new-prev-buses (append prev-buses (list bus))]
           [new-t (find-t new-prev-buses t inc)])
      (solve (cdr buses) new-prev-buses new-t (* inc (bus-id bus))))))

(define (find-t buses [t 0] [inc 1])
  (define all? (lambda (proc ls) (for/and ([x ls]) (proc x))))
  (define valid? (lambda (b) (= 0 (modulo (+ t (bus-offset b)) (bus-id b)))))
  (if (all? valid? buses)
      t
      (find-t buses (+ t inc) inc)))

(struct bus (id offset))

(define (parse-input input-file-path)
  (for/fold ([buses '()]
             [offset 0]
             #:result buses)
            ([id (string-split (cadr (file->lines input-file-path)) ",")])
    (if (string=? "x" id)
        (values buses
                (+ offset 1))
        (values (append buses (list (bus (string->number id) offset)))
                (+ offset 1)))))

(solve (parse-input "input.txt"))
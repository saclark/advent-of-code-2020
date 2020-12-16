#lang racket/base

(require racket/file
         racket/function
         racket/match
         racket/set
         racket/string
         rackunit)

(define (solve fields my-ticket nearby-tickets)
  (for/product ([(col candidates) (in-hash (refine-candidates-by-column (build-candidates-by-column fields my-ticket nearby-tickets)))])
    (if (string-prefix? (set-first candidates) "departure")
        (list-ref my-ticket col)
        1)))

(define (build-candidates-by-column fields my-ticket nearby-tickets)
  (define candidates-by-column (make-hasheq))
  (for ([col (in-range (length my-ticket))])
    (hash-set! candidates-by-column col (list->seteqv (map field-name fields))))
  (for ([(vals col) (in-indexed (build-values-by-column fields my-ticket nearby-tickets))])
    (for ([v vals])
      (for ([field fields])
        (when (not (valid-number? field v))
            (hash-update! candidates-by-column
                          col
                          (λ (candidates) (set-remove candidates (field-name field))))))))
  candidates-by-column)

(define (build-values-by-column fields my-ticket nearby-tickets)
  (define transpose (λ (xss) (apply map list xss)))
  (transpose (filter (curry valid-ticket? fields) (cons my-ticket nearby-tickets))))

(define (refine-candidates-by-column candidates-by-column)
  (let ([known-columns (find-known-columns candidates-by-column)])
    (if (= (length known-columns) (hash-count candidates-by-column))
        candidates-by-column
        (begin
          (for ([known known-columns])
            (for ([(col candidates) (in-hash candidates-by-column)])
              (when (not (= col (car known)))
                    (hash-update! candidates-by-column col (λ (set) (set-remove set (cdr known)))))))
          (refine-candidates-by-column candidates-by-column)))))

(define (find-known-columns candidates-by-column)
  (for/fold ([known '()])
            ([col (hash-keys candidates-by-column)])
    (let ([candidates (hash-ref candidates-by-column col)])
      (case (set-count candidates)
        [(0) (raise-user-error "Ya done messed up A-A-Ron!")]
        [(1) (cons (cons col (set-first candidates)) known)]
        [else known]))))

(struct field (name min1 max1 min2 max2))

(define (valid-ticket? fields ticket)
  (for/and ([num ticket])
    (for/or ([field fields]) (valid-number? field num))))

(define (valid-number? field n)
  (or (and ((field-min1 field) . <= . n) (n . <= . (field-max1 field)))
      (and ((field-min2 field) . <= . n) (n . <= . (field-max2 field)))))

(define (parse-input input-file-path)
  (match (string-split (file->string input-file-path) "\n\n")
    [(list fields-str my-ticket-str nearby-tickets-str)
      (values (parse-fields fields-str)
              (parse-ticket (cadr (string-split my-ticket-str "\n")))
              (map parse-ticket (cdr (string-split nearby-tickets-str "\n"))))]))

(define (parse-fields fields-str)
  (for/list ([line (string-split fields-str "\n")])
    (match-let ([(list name min1 max1 min2 max2)
                 (car (regexp-match* #rx"(.+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)"
                                     line
                                     #:match-select cdr))])
      (field name
            (string->number min1)
            (string->number max1)
            (string->number min2)
            (string->number max2)))))

(define (parse-ticket ticket-str)
  (map string->number (string-split ticket-str ",")))

(let-values ([(fields my-ticket nearby-tickets) (parse-input "input.txt")])
  (check-eqv? (solve fields my-ticket nearby-tickets) 5311123569883))
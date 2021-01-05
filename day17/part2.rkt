#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         rackunit
         threading)

(define (solve space cycles)
  (tree-leaf-count
    (for/fold ([s space])
              ([_ (in-range cycles)])
      (run-iteration s))))

(define (run-iteration space)
  (define new-space (make-hasheq))
  (for ([coord (in-list (all-space-coords space))])
    (when (make-active? space coord) (tree-add! new-space coord)))
  new-space)

(define (make-active? space coord)
  (let ([n (active-neighbor-count space coord)])
    (if (tree-has-branch? space coord)
      (if (or (= n 2) (= n 3)) #t #f)
      (if (not (= n 3)) #f #t))))

(define (active-neighbor-count space coord)
  (length (filter (curry tree-has-branch? space) (neighboring-coords coord))))

(define (all-space-coords space)
  (define coords (make-hasheq))
  (for ([coord (in-list (tree-branches space))])
    (tree-add! coords coord)
    (for ([neighbor (neighboring-coords coord)])
      (tree-add! coords neighbor)))
  (tree-branches coords))

(define (neighboring-coords coord)
  (let ([offsets (permutations (length coord) '(-1 0 1))])
    (for/fold ([acc '()])
              ([offset (in-list offsets)])
      (if (for/and ([o (in-list offset)]) (= 0 o))
          acc
          (cons (shift-coord coord offset) acc)))))

(define (permutations size elements)
  (if (zero? size)
      '(())
      (flatmap (λ (p) (map (λ (e) (cons e p)) elements))
               (permutations (sub1 size) elements))))

(define (flatmap proc ls)
  (foldl (λ (l acc) (append acc l)) '() (map proc ls)))

(define (shift-coord coord offsets)
  (for/fold ([acc '()]
             #:result (reverse acc))
            ([v (in-list coord)]
             [offset (in-list offsets)])
    (cons (+ v offset) acc)))

(define (tree-leaf-count tree)
  (for/fold ([count 0])
            ([(k v) (in-hash tree)])
    (let ([sub-count (tree-leaf-count v)])
      (if (zero? sub-count)
          (+ count 1)
          (+ count sub-count)))))

(define (tree-branches tree)
  (for/fold ([branches '()])
            ([(k v) (in-hash tree)])
    (let* ([sub-branches (tree-branches v)]
           [branch (if (null? sub-branches)
                       (list (list k))
                       (map (curry cons k) (tree-branches v)))])
      (append branches branch))))

(define (tree-has-branch? tree nodes)
  (if (null? nodes)
      #t
      (let ([t (hash-ref tree (car nodes) #f)])
        (if t (tree-has-branch? t (cdr nodes)) #f))))

(define (tree-add! tree nodes)
  (unless (null? nodes)
    (let* ([node (car nodes)]
           [sub-tree (hash-ref tree node #f)])
      (if sub-tree
          (tree-add! sub-tree (cdr nodes))
          (let ([sub-tree (make-hasheq)])
            (tree-add! sub-tree (cdr nodes))
            (hash-set! tree node sub-tree))))))

(define (parse-input dimensions input-file-path)
  (define chars->states (λ (chars) (map char->state chars)))
  (define char->state (λ (char) (match char [#\. #f] [#\# #t])))
  (~> input-file-path
      file->lines
      (map string->list _)
      (map chars->states _)
      (layer->space dimensions _)))

(define (layer->space dimensions layer)
  (let* ([pocket-space (make-hasheq)])
    (for ([(row y) (in-indexed layer)])
      (for ([(active? x) (in-indexed row)])
        (when active? (tree-add! pocket-space (append (list x y) (make-list (- dimensions 2) 0))))))
     pocket-space))

;;(check-eqv? (solve (parse-input 3 "input.txt") 6) 362)
(check-eqv? (solve (parse-input 4 "input.txt") 6) 1980)
;;(check-eqv? (solve (parse-input 5 "input.txt") 6) 13508)

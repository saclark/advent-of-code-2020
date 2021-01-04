#lang typed/racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         typed/rackunit
         threading)

(: solve (-> (Tree Integer) Natural Natural))
(define (solve space cycles)
  (tree-leaf-count
    (for/fold ([s : (Tree Integer) space])
              ([_ (in-range cycles)])
      (run-iteration s))))

(: run-iteration (-> (Tree Integer) (Tree Integer)))
(define (run-iteration space)
  (define new-space : (Tree Integer) (make-hasheq))
  (for ([coord (in-list (all-space-coords space))])
    (when (make-active? space coord) (tree-add! new-space coord)))
  new-space)

(: make-active? (-> (Tree Integer) (Listof Integer) Boolean))
(define (make-active? space coord)
  (let ([n (active-neighbor-count space coord)])
    (if (tree-has-branch? space coord)
      (if (or (= n 2) (= n 3)) #t #f)
      (if (not (= n 3)) #f #t))))

(: active-neighbor-count (-> (Tree Integer) (Listof Integer) Integer))
(define (active-neighbor-count space coord)
  (length (filter (curry (ann tree-has-branch? (-> (Tree Integer) (Listof Integer) Boolean)) space)
                  (neighboring-coords coord))))

(: all-space-coords (-> (Tree Integer) (Listof (Listof Integer))))
(define (all-space-coords space)
  (define coords : (Tree Integer) (make-hasheq))
  (for ([coord (in-list (tree-branches space))])
    (tree-add! coords coord)
    (for ([neighbor (neighboring-coords coord)])
      (tree-add! coords neighbor)))
  (tree-branches coords))

(: neighboring-coords (-> (Listof Integer) (Listof (Listof Integer))))
(define (neighboring-coords coord)
  (let ([offsets (permutations (length coord) '(-1 0 1))])
    (for/fold ([acc : (Listof (Listof Integer)) '()])
              ([offset (in-list offsets)])
      (if (ann (for/and ([o (in-list offset)]) (= 0 o)) Boolean)
          acc
          (cons (shift-coord coord offset) acc)))))

(: permutations (All (A) (-> Integer (Listof A) (Listof (Listof A)))))
(define (permutations size elements)
  (if (zero? size)
      '(())
      (flat-map (λ ([permutation : (Listof A)]) (map (λ ([elem : A]) (cons elem permutation)) elements))
               (permutations (sub1 size) elements))))

(: flat-map (All (A B) (-> (-> A (Listof B)) (Listof A) (Listof B))))
(define (flat-map proc ls)
  (for/fold ([acc : (Listof B) '()])
            ([l (in-list (map proc ls))])
    (append acc l)))

(: shift-coord (-> (Listof Integer) (Listof Integer) (Listof Integer)))
(define (shift-coord coord offsets)
  (for/fold ([acc : (Listof Integer) '()]
             #:result (reverse acc))
            ([v (in-list coord)]
             [offset (in-list offsets)])
    (cons (+ v offset) acc)))

(define-type (Tree A) (Rec T (HashTable A T)))

(: tree-leaf-count (All (A) (-> (Tree A) Natural)))
(define (tree-leaf-count tree)
  (for/fold ([count : Natural 0])
            ([([k : A] [v : (Tree A)]) (in-hash tree)])
    (let ([sub-count (tree-leaf-count v)])
      (if (zero? sub-count)
          (+ count 1)
          (+ count sub-count)))))

(: tree-branches (All (A) (-> (Tree A) (Listof (Listof A)))))
(define (tree-branches tree)
  (for/fold ([branches : (Listof (Listof A)) '()])
            ([([k : A] [v : (Tree A)]) (in-hash tree)])
    (let* ([sub-branches (tree-branches v)]
           [branch (if (null? sub-branches)
                       (list (list k))
                       (map (curry (ann cons (-> A (Listof A) (Listof A))) k) sub-branches))])
      (append branches branch))))

(: tree-has-branch? (All (A) (-> (Tree A) (Listof A) Boolean)))
(define (tree-has-branch? tree nodes)
  (if (null? nodes)
      #t
      (let ([sub-tree : (U (Tree A) False) (hash-ref tree (car nodes) #f)])
        (if sub-tree (tree-has-branch? sub-tree (cdr nodes)) #f))))

(: tree-add! (All (A) (-> (Tree A) (Listof A) Void)))
(define (tree-add! tree nodes)
  (unless (null? nodes)
    (let* ([node (car nodes)]
           [sub-tree : (U (Tree A) False) (hash-ref tree node #f)])
      (if sub-tree
          (tree-add! sub-tree (cdr nodes))
          (let ([sub-tree : (Tree A) (make-hasheq)])
            (tree-add! sub-tree (cdr nodes))
            (hash-set! tree node sub-tree))))))

(: parse-input (-> Integer String (Tree Integer)))
(define (parse-input dimensions input-file-path)
  (: char->state (-> Char Boolean))
  (define char->state (λ (char) (match char [#\. #f] [#\# #t])))
  (: chars->states (-> (Listof Char) (Listof Boolean)))
  (define chars->states (λ (chars) (map char->state chars)))
  (~> input-file-path
      file->lines
      (map string->list _)
      (map chars->states _)
      (layer->space dimensions _)))

(: layer->space (-> Integer (Listof (Listof Boolean)) (Tree Integer)))
(define (layer->space dimensions layer)
  (let* ([height (sub1 (length layer))]
         [width (sub1 (apply max (map (ann length (-> (Listof Boolean) Index)) layer)))]
         [pocket-space : (Tree Integer) (make-hasheq)])
    (for ([(row y) (in-indexed layer)])
      (for ([(active? x) (in-indexed row)])
        (when active?
          (tree-add! pocket-space
                     (append (list x y) (make-list (- dimensions 2) 0))))))
     pocket-space))

;;(check-eqv? (solve (parse-input 3 "input.txt") 6) 362)
(check-eqv? (solve (parse-input 4 "input.txt") 6) 1980)
;;(check-eqv? (solve (parse-input 5 "input.txt") 6) 13508)

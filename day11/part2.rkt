#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         racket/string
         rackunit
         threading)

(define (solve seating-chart)
  (for/sum ([row (fill-seats-until-stable seating-chart)])
    (count (curry eqv? 'occupied) row)))

(define (fill-seats-until-stable seating-chart #:visualize? [visualize? #f])
  (if visualize? (display-visualization-frame seating-chart) (void))
  (let-values ([(new-chart stable?) (fill-seats seating-chart)])
    (if stable?
        new-chart
        (fill-seats-until-stable new-chart #:visualize? visualize?))))

(define (fill-seats seating-chart)
  (for/fold ([chart-acc '()]
             [chart-stable? #t])
            ([y (length seating-chart)])
    (let-values ([(new-row new-row-stable?)
                  (for/fold ([row-acc '()]
                             [row-stable? #t])
                            ([x (length (list-ref seating-chart y))])
                    (let-values ([(new-state flipped?) (set-state seating-chart (cons x y))])
                      (values (append row-acc (list new-state))
                              (and row-stable? (not flipped?)))))])
      (values (append chart-acc (list new-row))
              (and chart-stable? new-row-stable?)))))

(define (set-state seating-chart coord)
  (let ([state (lookup-state seating-chart coord)])
    (match state
      ['floor (values 'floor #f)]
      ['wall (values 'wall #f)]
      ['empty (if (= (count-occupied-neighbors seating-chart coord) 0)
                  (values 'occupied #t)
                  (values 'empty #f))]
      ['occupied (if (>= (count-occupied-neighbors seating-chart coord) 5)
                     (values 'empty #t)
                     (values 'occupied #f))])))

(define (lookup-state seating-chart coord)
  (let ([x (car coord)]
        [y (cdr coord)])
    (if (or (< y 0) (>= y (length seating-chart)))
        'wall
        (let ([row (list-ref seating-chart y)])
          (if (or (< x 0) (>= x (length row)))
              'wall
              (list-ref row x))))))

(define (count-occupied-neighbors seating-chart coord)
  (count (curry eqv? 'occupied) (surrounding-states seating-chart coord)))

(define (find-state seating-chart coord x+ y+)
  (define move (λ (coord x+ y+) (cons (+ (car coord) x+) (+ (cdr coord) y+))))
  (let* ([new-coord (move coord x+ y+)]
         [state (lookup-state seating-chart new-coord)])
    (match state
      ['wall 'wall]
      ['floor (find-state seating-chart new-coord x+ y+)]
      [else state])))

(define (surrounding-states seating-chart coord)
  (list (find-state seating-chart coord -1 -1)
        (find-state seating-chart coord 0 -1)
        (find-state seating-chart coord 1 -1)
        (find-state seating-chart coord 1 0)
        (find-state seating-chart coord 1 1)
        (find-state seating-chart coord 0 1)
        (find-state seating-chart coord -1 1)
        (find-state seating-chart coord -1 0)))

(define (parse-input input-file-path)
  (define char->seat-state (λ (char) (match char [#\. 'floor] [#\L 'empty] [#\# 'occupied])))
  (define chars->seat-states (λ (chars) (map char->seat-state chars)))
  (~> input-file-path
      file->lines
      (map string->list _)
      (map chars->seat-states _)))

(define (seating-chart->string seating-chart)
  (define symbol->char (λ (sym) (match sym ['floor "."] ['empty "\x1b[32mL\x1b[0m"] ['occupied "\x1b[31m#\x1b[0m"])))
  (define row->string (λ (row) (string-join (map symbol->char row))))
  (string-join (map row->string seating-chart) "\n"))

(define (display-visualization-frame seating-chart)
  (display "\r")
  (display (seating-chart->string seating-chart))
  (display (format "\033[~aA" (- (length seating-chart) 1)))
  (sleep 0.2))

;; Solution
(check-eqv? (solve (parse-input "input.txt")) 2085)

;; Visualization
;; (void
;;   (begin
;;     (fill-seats-until-stable (parse-input "visualization-input.txt") #:visualize? #t))
;;     (display "\r"))

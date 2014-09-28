#lang racket/base

;;;; Functions for controlling a minesweeper field and the data inside of it.

(require racket/match
         racket/list
         racket/function
         racket/serialize
         "grid.rkt")

(provide cell-mine?
         cell-marked?
         cell-hidden?
         cell-reveal-propogate
         count-adjacent-mines
         make-minefield
         game-over?)

(serializable-struct cell (type marked? hidden?) #:transparent)

(define (make-cell type)
  (cell type #f #t))

(define (cell-reveal a-cell)
  (match-define (cell type _ _) a-cell)
  (cell type #f #f))

(define (can-propogate? a-grid x y)
  (and (not (cell-mine? (grid-ref a-grid x y)))
       (andmap (negate cell-mine?)
               (for/list ([position (adjacent-positions x y)]
                          #:when (and (member (first position) (range (grid-width a-grid)))
                                      (member (second position) (range (grid-height a-grid)))))
                 (grid-ref a-grid (first position) (second position))))))
                 

(module+ test
  (let ([a-grid (make-grid (list (list (make-cell 'clear) (make-cell 'clear) (make-cell 'clear))
                                 (list (make-cell 'clear) (make-cell 'clear) (make-cell 'clear))
                                 (list (make-cell 'clear) (make-cell 'clear) (make-cell 'mine))))])
    (check-true (can-propogate? a-grid 0 0))
    (check-false (can-propogate? a-grid 2 2))
    (check-false (can-propogate? a-grid 1 1))))

(define (cleave val lst)
  (if (null? lst)
      val
      (cleave ((first lst) val) (rest lst))))

(module+ test
  (check-equal? (cleave 0 (list add1 add1 add1)) 3)
  (check-equal? (cleave 10 (list (curry / 5) (curry / 2))) 4))

(define (cell-reveal-propogate a-grid x y)
  (if (can-propogate? a-grid x y)
      (cleave (grid-update a-grid x y cell-reveal)
              (for/list ([position (adjacent-positions x y)]
                         #:when (and (member (first position) (range (grid-width a-grid)))
                                     (member (second position) (range (grid-height a-grid)))
                                     (cell-hidden? (grid-ref a-grid (first position) (second position)))))
                (λ (a-grid)
                  (cell-reveal-propogate a-grid (first position) (second position)))))
      (grid-update a-grid x y cell-reveal)))

(module+ test
  (let ([a-grid (make-grid (list (list (make-cell 'clear) (make-cell 'clear) (make-cell 'clear))
                                 (list (make-cell 'clear) (make-cell 'clear) (make-cell 'clear))
                                 (list (make-cell 'clear) (make-cell 'clear) (make-cell 'mine))))])
    (check-equal? (cell-reveal-propogate a-grid 1 1) (grid-update a-grid 1 1 cell-reveal))
    (check-false (cell-hidden? (grid-ref (cell-reveal-propogate a-grid 0 0) 2 1)))))
    

(define (cell-mine? a-cell)
  (eq? (cell-type a-cell) 'mine))

(define (make-minefield width height mine-rate)
  (make-grid
   (for/list ([y height])
     (for/list ([x height])
       (if (> (random) mine-rate)
           (make-cell 'clear)
           (make-cell 'mine))))))

(module+ test
  (require rackunit)
  (check-equal? (make-minefield 2 2 -0.000001) (make-grid (list (list (make-cell 'clear) (make-cell 'clear))
                                                                (list (make-cell 'clear) (make-cell 'clear)))))
  (check-equal? (make-minefield 2 2 1.0000001) (make-grid (list (list (make-cell 'mine) (make-cell 'mine))
                                                                (list (make-cell 'mine) (make-cell 'mine))))))

(define (adjacent-positions x y)
  (list (list (add1 x) (add1 y))
        (list (add1 x) (sub1 y))
        (list (sub1 x) (add1 y))
        (list (sub1 x) (sub1 y))
        (list (add1 x) y)
        (list (sub1 x) y)
        (list x (add1 y))
        (list x (sub1 y))))

(module+ test
  (check-equal? (adjacent-positions 5 5)
                (list (list 6 6)
                      (list 6 4)
                      (list 4 6)
                      (list 4 4)
                      (list 6 5)
                      (list 4 5)
                      (list 5 6)
                      (list 5 4))))

(define (count-adjacent-mines a-grid x y)
  (for/fold ([sum 0])
            ([position (adjacent-positions x y)])
    (match-define (list iter-x iter-y) position)
    (if (and (member iter-x (range (grid-width a-grid)))
             (member iter-y (range (grid-height a-grid)))
             (cell-mine? (grid-ref a-grid iter-x iter-y)))
        (add1 sum)
        sum)))  

(module+ test
  (let ([a-grid (make-grid (list (list (make-cell 'mine) (make-cell 'mine) (make-cell 'clear))
                                 (list (make-cell 'mine) (make-cell 'clear) (make-cell 'clear))
                                 (list (make-cell 'clear) (make-cell 'mine) (make-cell 'clear))))])
    (check-equal? (count-adjacent-mines a-grid 1 1) 4)
    (check-equal? (count-adjacent-mines a-grid 0 0) 2)
    (check-equal? (count-adjacent-mines a-grid 2 2) 1)))

(define (game-over? a-grid)
  (grid-fold (λ (accum iter-value iter-x iter-y)
               (or accum (and (cell-mine? iter-value)
                              (not (cell-hidden? iter-value)))))
             #f
             a-grid))
             

(module+ test
  (let ([a-grid (make-grid (list (list (make-cell 'mine))
                                 (list (make-cell 'mine))))])
    (check-false (game-over? a-grid))
    (check-true (game-over? (grid-update a-grid 0 0 cell-reveal)))))
              
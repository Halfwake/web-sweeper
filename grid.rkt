#lang racket/base

;;;; Functional 2D grid data structure.
;;;; Remember: a nested list/vector/collection/array is not a 2D grid!
;;;; TODO: Rewrite to use vectors instead of lists. Might need a third party library.

(require racket/function
         racket/serialize)

(provide make-grid
         grid-rows
         grid-columns
         grid-row
         grid-column
         grid-width
         grid-height
         grid-fold
         grid-map
         grid-ref
         grid-update
         grid-set)

(serializable-struct grid (rows) #:transparent)

(define (make-grid rows)
  (cond [(not (apply = (map length rows)))
         (raise (exn:fail:contract "rows of different lengths"))]
        [(null? rows)
         (raise (exn:fail:contract "0 width grid"))]
        [(null? (list-ref rows 0))
         (raise (exn:fail:contract "0 height grid"))]
        [else
         (grid rows)]))

(module+ test
  (require rackunit)
  (check-exn
   exn:fail:contract?
   (thunk (make-grid '())))
  (check-exn
   exn:fail:contract?
   (thunk (make-grid '(() ()))))
  (check-exn
   exn:fail:contract?
   (thunk (make-grid '((1 2 3) (1 2) (1))))))

(define (transpose lst)
  (apply map list lst))

(define (grid-columns a-grid)
  (transpose (grid-rows a-grid)))

(module+ test
  (check-equal? (transpose '((1 2) (3 4))) '((1 3) (2 4)))
  (let ([a-grid (make-grid '((1 2 3) (4 5 6)))])
    (check-equal? (grid-rows a-grid) '((1 2 3) (4 5 6)))
    (check-equal? (grid-columns a-grid) '((1 4) (2 5) (3 6)))))

(define (grid-row a-grid n)
  (list-ref (grid-rows a-grid) n))

(define (grid-column a-grid n)
  (list-ref (grid-columns a-grid) n))

(module+ test
  (let ([a-grid (make-grid '((1 2 3) (4 5 6)))])
    (check-equal? (grid-row a-grid 0) '(1 2 3))
    (check-equal? (grid-row a-grid 1) '(4 5 6))
    (check-equal? (grid-column a-grid 0) '(1 4))
    (check-equal? (grid-column a-grid 2) '(3 6))))

(define (grid-width a-grid)
  (length (grid-columns a-grid)))

(define (grid-height a-grid)
  (length (grid-rows a-grid)))

(module+ test
  (let ([a-grid (make-grid '((0 0 0) (1 2 3) (4 5 6) (7 8 9)))])
    (check-equal? (grid-width a-grid) 3)
    (check-equal? (grid-height a-grid) 4)))

(define (grid-fold func init a-grid)
  (for/fold ([accum init])
            ([iter-y (grid-height a-grid)]
             [iter-x (grid-width a-grid)])
    (define current-value (grid-ref a-grid iter-x iter-y))
    (func accum current-value iter-x iter-y)))

(module+ test
  (let ([a-grid (make-grid '((0 0 0) (0 0 0) (0 0 0) (0 0 0)))])
    (check-equal? (grid-fold (λ (accum _1 _2 _3)
                               (add1 accum))
                             0
                             a-grid)
                  3)))

(define (grid-map func a-grid)
  (make-grid
   (for/list ([iter-y (grid-height a-grid)])
     (for/list ([iter-x (grid-width a-grid)])
       (define current-value (grid-ref a-grid iter-x iter-y))
       (func current-value iter-x iter-y)))))

(module+ test
  (let ([a-grid (make-grid '((1 2 3) (4 5 6)))])
    (check-equal? (grid-map (const -1) a-grid) (make-grid '((-1 -1 -1) (-1 -1 -1))))
    (check-equal? (grid-map (lambda (value _1 _2) value) a-grid) a-grid)
    (check-equal? (grid-map (lambda (value _1 _2) (add1 value)) a-grid) (make-grid '((2 3 4) (5 6 7))))))

(define (grid-ref a-grid x y)
  (list-ref (grid-row a-grid y) x))

(module+ test
  (let ([a-grid (make-grid '((1 2 3) (4 5 6)))])
    (check-equal? (grid-ref a-grid 0 0) 1)
    (check-equal? (grid-ref a-grid 2 1) 6)
    (check-equal? (grid-ref a-grid 1 1) 5)))

(define (grid-update a-grid x y updater)
  (grid-map (lambda (iter-value iter-x iter-y)
              (if (and (= iter-x x) (= iter-y y))
                  (updater iter-value)
                  iter-value))
            a-grid))

(define (grid-set a-grid x y value)
  (grid-update a-grid x y (const value)))

(module+ test
  (let ([a-grid (make-grid '((1 2 3) (4 5 6)))])
    (check-equal? (grid-update a-grid 0 0 add1)
                  (make-grid '((2 2 3) (4 5 6))))
    (check-equal? (grid-update a-grid 0 0 (const 10))
                  (make-grid '((10 2 3) (4 5 6))))
    (check-equal? (grid-update (grid-update a-grid 0 0 add1) 0 0 sub1)
                  a-grid)
    (check-equal? (grid-set a-grid 0 0 10)
                  (grid-update a-grid 0 0 (const 10)))))


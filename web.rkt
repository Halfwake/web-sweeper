#lang racket/base

(require racket/list
         racket/match
         web-server/servlet
         web-server/servlet-env
         web-server/page
         "grid.rkt"
         "mine.rkt")

(struct game-state (grid))

(define (render-cell a-grid x y embed/url)
  (define a-cell (grid-ref a-grid x y))
  (cond [(cell-hidden? a-cell)
         `(a ([href ,(embed/url
                      (λ (req)
                        (grid-update a-grid x y cell-reveal)))])
             "X")]
        [(and (not (cell-hidden? a-cell))
              (cell-mine? a-cell))
         "*"]
        [(and (not (cell-hidden? a-cell))
              (not (cell-mine? a-cell)))
         (format "~a" (count-adjacent-mines a-grid x y))]))


(define (render-cells a-grid embed/url)
  (grid-map (λ (iter-value iter-x iter-y)
              `(td (render-cell a-grid iter-x iter-y embed/url)))))

(define (render-row a-row)
  `(tr ,@a-row))

(define (chunkify lst n)
  (if (null? lst)
      '()
      (cons (take n lst) (chunkify (drop n lst) n))))

(define (render-grid a-grid embed/url)
  `(table ,@(map render-row (chunkify (render-cells a-grid embed/url) (grid-width a-grid)))))


;; TODO Refactor
(define (play-game a-game-state)
  (match-define (game-state a-grid) a-game-state)
  (play-game
   (game-state
    (send/suspend/dispatch
     (λ (embed/url)
       (response/xexpr
        `(html (head (title "MineSweeper"))
               (body (h1 "MineSweeper")
                     (table
                      ,@(for/list ([y (grid-height a-grid)])
                          `(tr ,@(for/list ([x (grid-width a-grid)])
                                  `(td ,(cond [(cell-hidden? (grid-ref a-grid x y))
                                             `(a ([href ,(embed/url
                                                          (λ (req)
                                                            (grid-update a-grid x y cell-reveal)))])
                                                 "X")]
                                            [(and (not (cell-hidden? (grid-ref a-grid x y)))
                                                  (cell-mine? (grid-ref a-grid x y)))
                                             "*"]
                                            [(and (not (cell-hidden? (grid-ref a-grid x y)))
                                                  (not (cell-mine? (grid-ref a-grid x y))))
                                             (format "~a" (count-adjacent-mines a-grid x y))]))))))))))))))

(define (new-game req)
  (play-game (game-state (make-minefield 10 10 .2))))

(module+ main
  (serve/servlet new-game))
  
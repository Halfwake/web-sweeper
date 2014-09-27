#lang racket/base

(require racket/list
         racket/match
         web-server/servlet
         web-server/servlet-env
         web-server/page
         "grid.rkt"
         "mine.rkt")

(struct game-state (grid))

; DON'T NEED


(define (render-cells a-grid embed/url)
  (grid-map (λ (iter-value iter-x iter-y)
              `(td (render-cell a-grid iter-x iter-y embed/url)))))

(define (render-row a-row)
  `(tr ,@a-row))

(define (chunkify lst n)
  (if (null? lst)
      '()
      (cons (take n lst) (chunkify (drop n lst) n))))

;; DON'T NEED

(define (reveal-link a-grid x y)
  (λ (req)
    (cell-reveal-propogate a-grid x y)))

(define (render-cell a-grid x y)
  (define a-cell (grid-ref a-grid x y))
  (cond [(cell-hidden? a-cell) "X"]
        [(cell-mine? a-cell) "*"]
        [else (format "~a" (count-adjacent-mines a-grid x y))]))

(define (render-grid a-grid embed/url)
  (for/list ([y (grid-height a-grid)])
    `(tr ,@(for/list ([x (grid-width a-grid)])
             `(td ,(cond [(cell-hidden? (grid-ref a-grid x y))
                          `(a ([href ,(embed/url  (reveal-link a-grid x y))])
                              ,(render-cell a-grid x y))]
                         [else (render-cell a-grid x y)]))))))
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
                     (table ,@(render-grid a-grid embed/url))))))))))
                      

(define (new-game req)
  (play-game (game-state (make-minefield 10 10 .2))))

(module+ main
  (serve/servlet new-game))
  
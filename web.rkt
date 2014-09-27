#lang racket/base

(require racket/list
         racket/match
         web-server/servlet
         web-server/servlet-env
         web-server/page
         "grid.rkt"
         "mine.rkt")

(struct game-state (grid render-mode))

(define (reveal-link a-grid x y)
  (λ (req)
    (cell-reveal-propogate a-grid x y)))

(define (render-cell-text a-grid x y)
  (define a-cell (grid-ref a-grid x y))
  (cond [(cell-hidden? a-cell) "X"]
        [(cell-mine? a-cell) "*"]
        [else (format "~a" (count-adjacent-mines a-grid x y))]))

(define cell-images (hash 'hidden "/hidden.png"
                          'mine "/mine.png"
                          0 "/clear.png"
                          1 "/1.png"
                          2 "/2.png"
                          3 "/3.png"
                          4 "/4.png"
                          5 "/5.png"
                          6 "/6.png"
                          7 "/7.png"
                          8 "/8.png"))

(define (render-cell-image a-grid x y)
  (define a-cell (grid-ref a-grid x y))
  `(img ([src ,(cond [(cell-hidden? a-cell) (hash-ref cell-images 'hidden)]
                     [(cell-mine? a-cell) (hash-ref cell-images 'mine)]
                     [else (hash-ref cell-images (count-adjacent-mines a-grid x y))])])))

(define (render-cell a-grid x y render-mode)
  (match render-mode
    ['text (render-cell-text a-grid x y)]
    ['image (render-cell-image a-grid x y)]))

(define (render-grid a-grid render-mode embed/url)
  `(table ,@(for/list ([y (grid-height a-grid)])
              `(tr ,@(for/list ([x (grid-width a-grid)])
                       `(td ,(cond [(cell-hidden? (grid-ref a-grid x y))
                                    `(a ([href ,(embed/url  (reveal-link a-grid x y))])
                                        ,(render-cell a-grid x y render-mode))]
                                   [else (render-cell a-grid x y render-mode)])))))))

(define (play-game a-game-state)
  (match-define (game-state a-grid render-mode) a-game-state)
  (play-game
   (game-state
    (send/suspend/dispatch
     (λ (embed/url)
       (response/xexpr
        `(html (head (title "MineSweeper"))
               (body (h1 "MineSweeper")
                     ,(render-grid a-grid render-mode embed/url))))))
    render-mode)))
                      
(define (new-game req)
  (play-game (game-state (make-minefield 10 10 .05) 'image)))

(module+ main
  (serve/servlet new-game #:extra-files-paths (list "./images/")))
  
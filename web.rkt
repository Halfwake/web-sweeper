#lang web-server/base

(require racket/list
         racket/match
         web-server/lang/web
         web-server/http/xexpr
         web-server/servlet-env
         racket/serialize
         "grid.rkt"
         "mine.rkt")

(serializable-struct game-state (grid render-mode))

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

(define game-images (hash 'title "/title.png"
                          'new-game "/new_game.png"
                          'instructions "/instructions.png"
                          'winner "/winner.png"
                          'loser "/loser.png"))
                          

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

(define (new-game-link)
  (λ (req)
    (minefield-spawner)))

(define (render-new-game render-mode embed/url)
  `(a ([href ,(embed/url (new-game-link))])
      ,(match render-mode
         ['text "New Game"]
         ['image `(img ([src ,(hash-ref game-images 'new-game)]))])))
                      
(define (render-title render-mode)
  (match render-mode
    ['text "Minesweeper"]
    ['image `(img ([src ,(hash-ref game-images 'title)]))]))

(define (render-state-text a-grid)
  (cond [(won-game? a-grid) "You Won"]
        [(lost-game? a-grid) "You Lost"]))

(define (render-state-image a-grid)
  `(img ([src ,(cond [(won-game? a-grid) (hash-ref game-images 'winner)]
                     [(lost-game? a-grid) (hash-ref game-images 'loser)]
                     [else (hash-ref game-images 'instructions)])])))
                      

(define (render-state a-grid render-mode)
  (match render-mode
    ['text (render-state-text a-grid)]
    ['image (render-state-image a-grid)]))

(define (play-game a-game-state)
  (match-define (game-state a-grid render-mode) a-game-state)
  (play-game
   (game-state
    (send/suspend/dispatch
     (λ (embed/url)
       (response/xexpr
        `(html (head (title "MineSweeper"))
               (body ,(render-title render-mode)
                     ,(render-grid a-grid render-mode embed/url)
                     ,(render-state a-grid render-mode)
                     (br)
                     ,(render-new-game render-mode embed/url))))))
    render-mode)))

(define (minefield-spawner)
  (make-minefield 10 10 .05))
                      
(define (new-game req)
  (play-game (game-state (minefield-spawner) 'image)))

(module+ main
  (serve/servlet new-game #:extra-files-paths (list "./images/") #:stateless? #t))
  
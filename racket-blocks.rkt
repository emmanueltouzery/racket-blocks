#lang curly-fn racket

(require 2htdp/image 2htdp/universe)
(require "drawing.rkt" "game-logic.rkt")
(require curly-fn)

;; normal screen

(define (normal-handle-key game-state k)
  (case k
    [("left") (piece-move-x -1 game-state)]
    [("right") (piece-move-x 1 game-state)]
    [("down") (game-state-update-piece
               game-state
               piece-state-rotate-right)]
    [("up") (game-state-update-piece
               game-state
               piece-state-rotate-left)]
    [(" ") (lower-piece game-state tile-size)]
    [("p" "P")
     (game-state-mode-set game-state 'paused)]
    [else game-state]))

(struct screen (to-draw on-key on-tick))

(define normal-screen
  (screen draw-board-and-tiles
          normal-handle-key
          lower-piece))

;; pause screen

(define pause-screen
  (screen
   #{draw-text-overlay "Paused" (draw-board-and-tiles %)}
   (λ(game-state k)
     (case k
       [("p" "P") (game-state-mode-set game-state 'normal)]
       [else game-state]))
   identity))

;; wiping screen

(define (wiping-call action)
  (λ(game-state)
    (match (game-state-mode game-state)
      [(list 'wiping-rows idx-pics step)
       (action game-state idx-pics step)])))

(define (wiping-on-tick game-state idx-pics step)
  (wipe-rows-step game-state (map first idx-pics) step))

(define (wiping-on-draw game-state idx-pics step)
  (for/fold ([board (draw-board-and-tiles game-state)]) ([idx-pic idx-pics])
    (match idx-pic
      [(list idx pic)
       (place-image/align pic (- step)
                          (* tile-size (- board-height-tiles idx 1))
                          "left" "top" board)])))

(define wiping-rows-screen
  (screen
   (wiping-call wiping-on-draw)
   (λ(game-state k) game-state)
   (wiping-call wiping-on-tick)))

;; game over screen

(define game-over-screen
  (screen
   #{draw-text-overlay "Game over!" (draw-board-and-tiles %)}
   (λ(game-state k)
     (case k
       [(" ") start-game-state]
       [else game-state]))
   identity))

;; general code

(define (get-active-screen game-state)
  (match (game-state-mode game-state)
    ['normal normal-screen]
    ['paused pause-screen]
    ['game-over game-over-screen]
    [(list 'wiping-rows _ _) wiping-rows-screen]
    [else (printf "Unkwown game mode? ~a~n" game-state)]))

(define start-game-state
  (game-state
   'normal
   (get-new-piece)
   '() board-bg))

(define (with-screen screen-action)
  (λ params
    (apply
     (screen-action (get-active-screen (first params)))
     params)))

(big-bang
 start-game-state
 (to-draw (with-screen screen-to-draw))
 (on-tick (with-screen screen-on-tick) 0.015)
 (on-key (with-screen screen-on-key)))

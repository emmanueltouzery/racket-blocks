#lang racket

(require 2htdp/image 2htdp/universe)
(require "drawing.rkt" "game-logic.rkt")

(define (handle-key game-state k)
  (cond
    [(key=? k "left") (piece-move-x -1 game-state)]
    [(key=? k "right") (piece-move-x 1 game-state)]))

(define start-game-state
  (game-state
   get-new-piece
   '() (freeze (draw-board))))

(big-bang
 start-game-state
 (to-draw draw-game)
 (on-tick lower-piece 0.015)
 (on-key handle-key))
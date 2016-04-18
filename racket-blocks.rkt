#lang racket

(require 2htdp/image 2htdp/universe)
(require "drawing.rkt" "game-logic.rkt")

(define (handle-key game-state k)
  (cond
    [(key=? k "left") (piece-move-x -1 game-state)]
    [(key=? k "right") (piece-move-x 1 game-state)]))

(define (center-x-offset-tiles piece)
   (quotient
    (- board-width-tiles (piece-width-tiles piece))
    2))
(define start-game-state
  (game-state
   (cur-piece-state
    (first pieces)
    (freeze (draw-piece (first pieces)))
    (center-x-offset-tiles (first pieces)) 0)
   (list
    (list "red" "green" #f "blue")
    (list "yellow" #f #f "red" #f)) (freeze (draw-board))))

(big-bang
 start-game-state
 (to-draw draw-game)
 (on-tick lower-piece)
 (on-key handle-key))
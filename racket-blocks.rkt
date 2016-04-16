#lang racket

(require 2htdp/image 2htdp/universe)
(require match-plus threading)
(require "drawing.rkt")

(struct cur-piece-state (piece pic y))
(struct game-state (cur-piece-state board-rows cur-board-pic))

(define/match* (draw-game (game-state cur-piece-state board-rows cur-board-draw))
  (define cur-piece-width-tiles
    (~> cur-piece-state cur-piece-state-piece piece-width-tiles))
  (define x-offset-tiles
    (quotient (- board-width-tiles cur-piece-width-tiles) 2))
  (~>
   (paint-board board-rows)
   (place-image/align
    (cur-piece-state-pic cur-piece-state)
    (* x-offset-tiles tile-size)
    (cur-piece-state-y cur-piece-state) "left" "top" _)))

(define/match* (update-state (game-state (cur-piece-state piece piece-pic piece-y) board-rows cur-board-pic))
  (game-state (cur-piece-state piece piece-pic (+ piece-y 1)) board-rows cur-board-pic))

(big-bang (game-state (cur-piece-state (first pieces) (draw-piece (first pieces)) 0) empty (draw-board))
          (to-draw draw-game)
          (on-tick update-state))
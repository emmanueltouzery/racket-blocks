#lang curly-fn racket

(require 2htdp/image 2htdp/universe)
(require match-plus threading curly-fn)
(require alexis/util/struct)
(require "drawing.rkt")

(struct cur-piece-state (piece pic x-tiles y-pixels))
(define-struct-updaters cur-piece-state)
(struct game-state (cur-piece-state board-rows cur-board-pic))
(define-struct-updaters game-state)

(define/match* (draw-game (game-state cur-piece-state board-rows cur-board-draw))
  (define cur-piece-width-tiles
    (~> cur-piece-state cur-piece-state-piece piece-width-tiles))
  (define x-offset-tiles
    (+
     (cur-piece-state-x-tiles cur-piece-state)
     (quotient (- board-width-tiles cur-piece-width-tiles) 2)))
  (~>
   (paint-board board-rows)
   (place-image/align
    (cur-piece-state-pic cur-piece-state)
    (* x-offset-tiles tile-size)
    (cur-piece-state-y-pixels cur-piece-state) "left" "top" _)))

(define (game-state-update-piece game-state piece-updater)
  (game-state-cur-piece-state-set
   game-state
   (piece-updater (game-state-cur-piece-state game-state))))

(define (lower-piece game-state)
  (game-state-update-piece
   game-state #{cur-piece-state-y-pixels-update % add1}))

(define (piece-move-x offset game-state)
  (game-state-update-piece
   game-state #{cur-piece-state-x-tiles-update % #{+ offset}}))

(define (handle-key game-state k)
  (cond
    [(key=? k "left") (piece-move-x -1 game-state)]
    [(key=? k "right") (piece-move-x 1 game-state)]))

(define start-game-state
  (game-state
   (cur-piece-state (first pieces) (freeze (draw-piece (first pieces))) 0 0)
   empty (freeze (draw-board))))

(big-bang
 start-game-state
 (to-draw draw-game)
 (on-tick lower-piece)
 (on-key handle-key))
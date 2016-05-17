#lang curly-fn racket

(require 2htdp/image 2htdp/universe)
(require "drawing.rkt" "game-logic.rkt")
(require curly-fn)

(define (handle-key game-state k)
  (case k
    [("left") (piece-move-x -1 game-state)]
    [("right") (piece-move-x 1 game-state)]
    [("down") (game-state-update-piece
               game-state
               piece-state-rotate-right)]
    [("up") (game-state-update-piece
               game-state
               piece-state-rotate-left)]
    [(" ") (case (game-state-mode game-state)
             ['normal (lower-piece game-state tile-size)]
             ['game-over start-game-state]
             [else game-state])]
    [("p" "P")
     (game-state-mode-update
      game-state #{case %
                    ['paused 'normal]
                    [else 'paused]})]
    [else game-state]))

(define start-game-state
  (game-state
   'normal
   (get-new-piece)
   '() board-bg))

(big-bang
 start-game-state
 (to-draw draw-game)
 (on-tick #{match (game-state-mode %)
             ['game-over %]
             ['paused %]
             ['normal (lower-piece %)]
             [(list 'wiping-rows idx-pics step)
              (wipe-rows-step % (map first idx-pics) step)]} 0.015)
 (on-key handle-key))

#lang curly-fn racket

(require 2htdp/image 2htdp/universe)
(require match-plus threading curly-fn)
(require alexis/util/struct)
(require "drawing.rkt")

(provide
 game-state
 cur-piece-state
 piece-move-x
 lower-piece
 draw-game)

(struct cur-piece-state (piece pic x-tiles y-pixels))
(define-struct-updaters cur-piece-state)
(struct game-state (cur-piece-state board-rows cur-board-pic))
(define-struct-updaters game-state)

(define falling-speed 5)

(define/match* (draw-game (game-state cur-piece-state board-rows cur-board-draw))
  (define cur-piece-width-tiles
    (~> cur-piece-state cur-piece-state-piece piece-width-tiles))
  (~>
   (paint-board board-rows)
   (place-image/align
    (cur-piece-state-pic cur-piece-state)
    (* (cur-piece-state-x-tiles cur-piece-state) tile-size)
    (cur-piece-state-y-pixels cur-piece-state) "left" "top" _)))

(define (game-state-update-piece game-state piece-updater)
  (game-state-cur-piece-state-set
   game-state
   (piece-updater (game-state-cur-piece-state game-state))))

(define/match* (piece-depths (cur-piece-state (piece _ pos) _ x-tiles _))
  (for/fold ([result (make-immutable-hash)])
            ([item pos])
    (match item
       [(list x y) (let ([y+ (add1 y)])
                     (hash-update result (+ x x-tiles) #{if (> y+ %) y+ %} y+))])))

(define/match* (piece-touches-bottom (game-state piece-state board-rows _))
  (define piece-top-tiles (quotient (cur-piece-state-y-pixels piece-state) tile-size))
  (define cur-piece-depths (piece-depths piece-state))
  (define coordinates-under-piece
    (hash-map cur-piece-depths
              (λ(x y) (list x (- board-height-tiles (+ y piece-top-tiles 1))))))
  (or
   (ormap #{< (second %) 0} coordinates-under-piece)
   (ormap #{apply board-get-item board-rows %} coordinates-under-piece)))

;; TODO indexing in a list of list is a sure
;; sign I'm using the wrong data structure.
;; move to http://srfi.schemers.org/srfi-25/srfi-25.html ?
(define (board-get-item board-rows x y)
  (define rows (length board-rows))
  (if (> (add1 y) rows)
      #f
      (let ([row (list-ref board-rows (- rows y 1))])
        (if (> (add1 x) (length row))
            #f
            (list-ref row x)))))

(define (lower-piece game-state)
  (define new-state (game-state-update-piece
   game-state #{cur-piece-state-y-pixels-update % #{+ falling-speed}}))
  (if (piece-touches-bottom game-state)
      (print "reached bottom!")
      (print "no bottom"))
  new-state)

(define (modify-in-range value offset min max)
  (let ([new-value (+ offset value)])
    (cond
      [(<= new-value min) min]
      [(>= new-value max) max]
      [else new-value])))

(define (piece-move-x offset game-state)
  (define cur-piece-width
    (~> game-state
        game-state-cur-piece-state
        cur-piece-state-piece
        piece-width-tiles))
  (define max-offset (- board-width-tiles cur-piece-width))
  (game-state-update-piece
   game-state #{cur-piece-state-x-tiles-update
                % #{modify-in-range % offset 0 max-offset}}))
#lang curly-fn racket

(require 2htdp/image 2htdp/universe)
(require match-plus threading curly-fn)
(require alexis/util/struct)
(require "drawing.rkt")

(provide
 game-state
 get-new-piece
 cur-piece-state
 piece-move-x
 lower-piece
 draw-game)

(struct cur-piece-state (piece pic x-tiles y-pixels))
(define-struct-updaters cur-piece-state)
(struct game-state (cur-piece-state board-rows cur-board-pic))
(define-struct-updaters game-state)

(define falling-speed 1)

(define/match* (draw-game (game-state cur-piece-state board-rows cur-board-draw))
  (define cur-piece-width-tiles
    (~> cur-piece-state cur-piece-state-piece piece-width-tiles))
  (~>
   cur-board-draw
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
  ;; TODO surely can move to a more elegant and-like form.
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
      (reached-bottom game-state)
      new-state))

(define (center-x-offset-tiles piece)
   (quotient
    (- board-width-tiles (piece-width-tiles piece))
    2))

(define get-new-piece
  (cur-piece-state
    (first pieces)
    (freeze (draw-piece (first pieces)))
    (center-x-offset-tiles (first pieces)) 0))

;; returns a hash with absolute grid positions
;; for the piece. key = y, value = list of x for that y.
(define/match* (get-piece-yx-positions (piece _ pos) left top)
  (for/fold ([result (make-immutable-hash)])
            ([item pos])
    (match item
       [(list x y)
        (let ([x+ (+ x left)])
          (hash-update
           result
           (- board-height-tiles (+ y top) 1)
           #{cons x+} (list x+)))])))

(define (board-update-row row color piece-x-list)
  (print piece-x-list)
  (for/list ([(cell idx) (in-indexed row)])
    (if (ormap #{= idx} piece-x-list)
        color cell)))

;; list of indexes & value to list with values
;; at those indexes.
(define (indexes-val-to-list length indexes val)
  (for/list ([idx (range length)])
    (if (ormap #{= idx} indexes) val #f)))

(define/match* (reached-bottom
                (game-state
                 (cur-piece-state piece _ x-tiles pc-y-pixels)
                 board-rows _))
  (define colr (piece-color piece))
  (define piece-top-tiles (quotient pc-y-pixels tile-size))
  (define piece-yx-pos
    (get-piece-yx-positions piece x-tiles piece-top-tiles))
  ;; update the existing rows
  (define updated-board-rows
    (reverse
     (for/list
         ([(row idx) (in-indexed (reverse board-rows))])
       (board-update-row
        row colr
        (hash-ref piece-yx-pos idx empty)))))
  ;; create new rows if needed
  (define new-board-rows
    (for/fold
     ([r updated-board-rows])
     ([idx (sort (hash-keys piece-yx-pos) <)]
      #:when (>= idx (length board-rows)))
      (cons
       (indexes-val-to-list
        board-width-tiles
        (hash-ref piece-yx-pos idx) colr)
       r)))
  ;; TODO wipe out rows that are now complete
  (game-state get-new-piece new-board-rows
              (paint-board new-board-rows)))

(define (modify-in-range value offset min max)
  (let ([new-value (+ offset value)])
    (cond
      [(<= new-value min) min]
      [(>= new-value max) max]
      [else new-value])))

;; TODO prevent moving left or right if there is a piece there.
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
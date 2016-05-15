#lang curly-fn racket

(require 2htdp/image 2htdp/universe)
(require match-plus threading curly-fn)
(require alexis/util/struct)
(require "drawing.rkt")
(require racket/contract)

(provide
 game-state
 game-state-mode
 game-state-mode-update
 game-state-update-piece
 piece-state-rotate-left
 piece-state-rotate-right
 get-new-piece
 cur-piece-state
 piece-move-x
 lower-piece
 draw-game
 wipe-rows-step)

(struct cur-piece-state (piece pic x-tiles y-pixels) #:transparent)
(define-struct-updaters cur-piece-state)
;; game-mode can be 'normal, 'paused, 'wiping-rows
(struct game-state (mode cur-piece-state board-rows cur-board-pic) #:transparent)
(define-struct-updaters game-state)

(define falling-speed 1)
(define move-x-tolerance 5)

;; TODO animation when we're in pause.
(define/match* (draw-game (game-state mode cur-piece-state board-rows cur-board-draw))
  (define cur-piece-width-tiles
    (~> cur-piece-state cur-piece-state-piece piece-width-tiles))
  (~>
   cur-board-draw
   (place-image/align
    (cur-piece-state-pic cur-piece-state)
    (* (cur-piece-state-x-tiles cur-piece-state) tile-size)
    (cur-piece-state-y-pixels cur-piece-state) "left" "top" _)
   (draw-mode-specific mode)))

(define (draw-mode-specific image mode)
  (match mode
    [(list 'wiping-rows rows step)
     (for/fold ([board image]) ([row rows])
       (paint-row (- step) board (second row) (add1 (first row))))]
    [else image]))

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

(define/match* (piece-touches-bottom (game-state _ piece-state board-rows _))
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
  (and (<= (add1 y) rows)
       (let ([row (list-ref board-rows (- rows y 1))])
         (and (<= (add1 x) (length row))
              (list-ref row x)))))

;; CAREFUL if you give falling-speed bigger
;; than tile-size you can overshoot and let it through!
(define/contract (lower-piece game-state [y-value falling-speed])
  (->* (game-state?)
       ((integer-in 0 tile-size))
       game-state?)
  (define new-state (game-state-update-piece
                     game-state #{cur-piece-state-y-pixels-update % #{+ y-value}}))
  (if (piece-touches-bottom game-state)
      (reached-bottom game-state)
      new-state))

(define (center-x-offset-tiles piece)
  (quotient
   (- board-width-tiles (piece-width-tiles piece))
   2))

(define get-new-piece
  (λ()
    (let
        ([piece (list-ref pieces (random (length pieces)))])
      (cur-piece-state
       piece (freeze (draw-piece piece))
       (center-x-offset-tiles piece) 0))))

;; returns a hash with absolute grid positions
;; for the piece. key = y, value = list of x for that y.
(define/match* (get-piece-yx-positions
                (cur-piece-state (piece _ pos) _ left y-pixels))
  (define top (quotient y-pixels tile-size))
  (for/fold ([result (make-immutable-hash)])
            ([item pos])
    (match item
      [(list x y)
       (let ([x+ (+ x left)])
         (hash-update
          result
          (- board-height-tiles (+ y top) 1)
          #{cons x+} '()))])))

(define (board-update-row row color piece-x-list)
  (for/list ([(cell idx) (in-indexed row)])
    (if (ormap #{= idx} piece-x-list)
        color cell)))

;; list of indexes & value to list with values
;; at those indexes.
(define (indexes-val-to-list length indexes val)
  (for/list ([idx (range length)])
    (if (ormap #{= idx} indexes) val #f)))

(define/match* (reached-bottom
                (game-state _ piece-state board-rows _))
  (define colr (piece-color
                (cur-piece-state-piece piece-state)))
  (define piece-yx-pos
    (get-piece-yx-positions piece-state))
  (define new-board
    (~>
     board-rows
     (board-update-rows colr piece-yx-pos)
     (board-create-new-rows-if-needed colr piece-yx-pos)))
  (reached-bottom-get-new-game-state new-board))

(define (board-update-rows board-rows colr piece-yx-pos)
  (reverse
   (for/list
       ([(row idx) (in-indexed (reverse board-rows))])
     (board-update-row
      row colr
      (hash-ref piece-yx-pos idx empty)))))

(define (board-create-new-rows-if-needed board-rows colr piece-yx-pos)
  (for/fold
   ([r board-rows])
   ([idx (sort (hash-keys piece-yx-pos) <)]
    #:when (>= idx (length board-rows)))
    (cons
     (indexes-val-to-list
      board-width-tiles
      (hash-ref piece-yx-pos idx) colr)
     r)))

(define (reached-bottom-get-new-game-state board)
  (define-values (new-board wiped-rows)
    (get-new-board-wiped-rows board))
  (define game-mode
    (if (null? wiped-rows)
        ;; no wiped rows, continue normally
        'normal
        ;; wiped rows, animate their removal
        (list 'wiping-rows wiped-rows 0)))
  (game-state
   game-mode
   (get-new-piece) new-board
   (freeze (paint-board new-board))))

(define (get-new-board-wiped-rows board)
  (for/fold
      ([new-board '()]
       [wiped-rows '()])
      ([i (in-naturals)]
       [row (reverse board)])
    (if (andmap identity row)
        ;; this row got wiped.
        (values (cons '() new-board) (cons (list i row) wiped-rows))
        ;; this row didn't get wiped.
        (values (cons row new-board) wiped-rows))))

(define (modify-in-range value offset min max)
  (let ([new-value (+ offset value)])
    (cond
      [(<= new-value min) min]
      [(>= new-value max) max]
      [else new-value])))

(define/match* (exactly-on-y? (cur-piece-state piece pic x-tiles y-pixels))
  (define diff (modulo y-pixels tile-size))
  (cond
    [(<= diff move-x-tolerance) 'cur]
    [(>= diff (- tile-size move-x-tolerance)) 'next]
    [else #f]))

(define (piece-move-x offset g-state)
  (printf "piece-move-x!!~n")
  (match-let
      ([(game-state _ piece-state board-rows _) g-state])
    (define piece-yx
      (hash->list (get-piece-yx-positions piece-state)))
    (define occupied-cur
      (occupied-pieces? board-rows offset 0 piece-yx))
    (printf "exactly on? ~a  -- occupied-cur ~a  ~n" (exactly-on-y? piece-state) occupied-cur)
    (define occupied
      (case (exactly-on-y? piece-state)
        ['cur occupied-cur]
        ['next (occupied-pieces? board-rows offset -1 piece-yx)]
        [else (or occupied-cur
                  (occupied-pieces? board-rows offset -1 piece-yx))]))
    (printf "occupied is ~a ~n" occupied)
    (if occupied
        g-state
        (do-move-x offset g-state))))

(define (occupied-pieces? board-rows offset-x offset-y piece-yx)
  (ormap #{position-occupied? board-rows offset-x offset-y} piece-yx))

(define (do-move-x offset game-state)
  (define cur-piece-width
    (~> game-state
        game-state-cur-piece-state
        cur-piece-state-piece
        piece-width-tiles))
  (define max-offset (- board-width-tiles cur-piece-width))
  (game-state-update-piece
   game-state #{cur-piece-state-x-tiles-update
                % #{modify-in-range % offset 0 max-offset}}))

(define/match* (position-occupied? board-rows offset-x offset-y (cons y xs))
  (define cmp (if (> offset-x 0) max min))
  (board-get-item board-rows (+ offset-x (apply cmp xs)) (+ offset-y y)))

(define (game-state-update-picture game-state)
  (game-state-cur-board-pic-set
   game-state
   (freeze (paint-board (game-state-board-rows game-state)))))

(define (wipe-rows-step game-state index+row-list step)
  (if (< step (* board-width-tiles tile-size))
      ;; increase the step
      (game-state-mode-update game-state #{list-update % 2 #{+ 10}})
      ;; we're done: complete the wipe.
      (complete-the-wipe game-state index+row-list)))

(define (complete-the-wipe game-state index+row-list)
  (define row-indexes (map first index+row-list))
  (~>
   ;; remove the rows that were wiped
   (game-state-board-rows-set game-state
    (reverse
     (for/list
         ([item (reverse (game-state-board-rows game-state))]
          [i (in-naturals)]
          #:when (not (member i row-indexes)))
       item)))
   ;; recalculate the picture, reset to normal.
   (game-state-update-picture)
   (game-state-mode-set 'normal)))

(define (piece-state-rotate-right piece-state)
  (define height
    (piece-height-tiles
     (cur-piece-state-piece piece-state)))
  (piece-state-transform-piece
   #{map (match-lambda [(list x y) (list (- height y) x)])}
   piece-state))

(define (piece-state-rotate-left piece-state)
  (define width
    (piece-width-tiles
     (cur-piece-state-piece piece-state)))
  (piece-state-transform-piece
   #{map (match-lambda [(list x y) (list y (- width x))])}
   piece-state))

;; make sure the x starts at 0 and the y too.
;; otherwise for the x, we can't move the piece
;; fully to the left of the screen anymore!
;; TODO will crash if list is empty?
(define/match* (piece-positions-reset-to-root (piece col positions))
  (define-values (min-x min-y)
    (for/fold
        ([cur-min-x (first (first positions))]
         [cur-min-y (second (first positions))])
        ([pos positions])
      (values
       (min (first pos) cur-min-x)
       (min (second pos) cur-min-y))))
  (define new-pos
    (for/list ([pos positions])
      (list (- (first pos) min-x) (- (second pos) min-y))))
  (piece col new-pos))

(define/match* (piece-state-transform-piece
                f (cur-piece-state piece pic x-tiles y-pixels))
  (define rotated-piece
    (piece-positions-reset-to-root
     (piece-positions-update piece f)))
  (cur-piece-state
   rotated-piece
   (freeze (draw-piece rotated-piece))
   x-tiles y-pixels))

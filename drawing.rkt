#lang curly-fn racket

(provide
 tile-size
 ;; board
 paint-board
 draw-board
 board-width-tiles
 board-height-tiles
 ;; pieces
 piece
 piece-color
 pieces
 draw-piece
 piece-width-tiles)

(require 2htdp/image lang/posn picturing-programs)
(require match-plus curly-fn threading)
(require (for-syntax syntax/parse))

(define tile-size 24)
(define tile-padding 1)
(define board-width-tiles 10)
(define board-height-tiles 25)
(define board-bgcolor "dark gray")
(define board-grid-color "light gray")

(define (make-gray x [a 255]) (make-color x x x a))

;; draw a rectangle with a gradient
(define (gradient-rect w h color)
  (define (apply-factor x) (* x (quotient 255 w)))
  (define base-rect (rectangle w h "solid" color))
  ;; measurably helps with perf to pre-calculate those
  (define col-vec (vector->immutable-vector (list->vector (foldl
                    (λ(x l) (cons (make-gray (apply-factor x) 127) l))
                    '() (range w)))))
  (overlay
   (map-image (λ(x y col) (vector-ref col-vec x)) base-rect)
   base-rect))

;; I need a macro for this because I want to get rid of the
;; make-posn calls everywhere. I can make a function using map
;; and "apply make-posn" but then instead of "make-posn" I have
;; to write "list" in my expressions so I don't gain much...
(define-syntax (make-posns stx)
  (define-syntax-class posn-pair
    #:description "posn pair"
    (pattern (x:expr y:expr)))
  (syntax-parse stx
    [(_ (p:posn-pair ...+))
     #'(list (make-posn p.x p.y) ...)]))

;; draw a single tile of a piece.
(define (block-tile color)
  (define bright-shade (make-gray 255 140))
  (define dark-shade (make-gray 0 140))
  (define overlay-width (quotient tile-size 7))
  (underlay
   ;; the background
   (gradient-rect tile-size tile-size color)
   ;; the brighter top-left shade
   (polygon
    (make-posns
     ((tile-size 0)
      (tile-size tile-size)
      (0 tile-size)
      (overlay-width (- tile-size overlay-width))
      ((- tile-size overlay-width) (- tile-size overlay-width))
      ((- tile-size overlay-width) overlay-width)))
    "solid" dark-shade)
   ;; the darker bottom-right shade
   (polygon
    (make-posns
     ((0 0)
      (tile-size 0)
      ((- tile-size overlay-width) overlay-width)
      (overlay-width overlay-width)
      (overlay-width (- tile-size overlay-width))
      (0 tile-size)))
    "solid" bright-shade)))

(struct piece (color positions))

(define pieces
  (list (piece "red"    '((0 0) (1 0) (2 0)
                                (1 1)))
        (piece "blue"   '((0 0) (1 0) (2 0) (3 0)))
        (piece "green"  '((0 0) (1 0)
                                (1 1) (2 1)))
        (piece "yellow" '((0 0) (1 0)
                          (0 1) (1 1)))
        (piece "orange" '((0 0) (1 0) (2 0)
                          (0 1)))))

;; calculates the offset of a tile for a position,
;; in pixels. Depends on the tile size and the padding between tiles.
(define (offset-px tile-count)
  (+ (* tile-count tile-size) (* (max 0 (sub1 tile-count)) tile-padding)))

(define/match* (piece-width-tiles (piece _ pos))
  (add1 (apply max (map first pos))))

(define/match* (piece-height-tiles (piece _ pos))
  (add1 (apply max (map second pos))))

(define (draw-piece piece)
  (define canvas
    (rectangle
     (offset-px (piece-width-tiles piece))
     (offset-px (piece-height-tiles piece))
     "solid" "transparent"))
  (define tile (block-tile (piece-color piece)))
  (foldl (λ(cur-pos c)
           (place-image/align
            tile
            (offset-px (first cur-pos))
            (offset-px (second cur-pos))
            "left" "top" c))
         canvas (piece-positions piece)))

;; draw the board (without any pieces or tiles)
(define (draw-board)
  (define board-width (* tile-size board-width-tiles))
  (define board-height (* tile-size board-height-tiles))
  (define offsets (stream-map #{* tile-size} (in-naturals)))
  (define (for-offsets board max-val action)
    (for/fold ([bg board]) ([val offsets]
        #:break (> val max-val))
    (action bg val)))
  (~>
   (gradient-rect board-width board-height board-bgcolor)
   (for-offsets board-width
                (λ(board x) (add-line board x 0 x board-height board-grid-color)))
   (for-offsets board-height
                (λ(board y) (add-line board 0 y board-width y board-grid-color)))))

;; paint the tiles listed in 'row' at y offset based on 'row-idx'
(define (paint-row board row row-idx)
  (define y (- (image-height board) (* tile-size row-idx)))
  (for/fold ([cur-board board]) ([cell-value row] [col-idx (in-naturals)])
    (cond
      [(image-color? cell-value)
       (define x (* tile-size col-idx))
       (place-image/align (block-tile cell-value) x y "left" "top" cur-board)]
      [else cur-board])))

;; paint a board with all the tiles as described by 'board-st'
(define (paint-board board-st)
  (for/fold ([board (draw-board)]) ([row board-st] [row-idx (in-naturals)])
    (paint-row board row (- (length board-st) row-idx))))
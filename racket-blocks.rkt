#lang curly-fn racket

(require 2htdp/universe 2htdp/image lang/posn picturing-programs)
(require match-plus curly-fn threading)

(define tile-size 24)
(define tile-padding 1)
(define board-width-tiles 10)
(define board-height-tiles 25)
(define board-bgcolor "dark gray")
(define board-grid-color "light gray")

(define (make-gray x [a 255]) (make-color x x x a))

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

(define (block-tile color)
  (define bright-shade (make-gray 255 140))
  (define dark-shade (make-gray 0 140))
  (define overlay-width (quotient tile-size 7))
  (underlay (gradient-rect tile-size tile-size color)
      (polygon (list (make-posn tile-size 0)
                     (make-posn tile-size tile-size)
                     (make-posn 0 tile-size)
                     (make-posn overlay-width (- tile-size overlay-width))
                     (make-posn (- tile-size overlay-width) (- tile-size overlay-width))
                     (make-posn (- tile-size overlay-width) overlay-width)) "solid" dark-shade)
      (polygon (list (make-posn 0 0)
                     (make-posn tile-size 0)
                     (make-posn (- tile-size overlay-width) overlay-width)
                     (make-posn overlay-width overlay-width)
                     (make-posn overlay-width (- tile-size overlay-width))
                     (make-posn 0 tile-size)) "solid" bright-shade)))


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

(define (offset-px tile-count)
  (+ (* tile-count tile-size) (* (max 0 (sub1 tile-count)) tile-padding)))

(define/match* (draw-piece (piece col pos))
  (define tiles-width (add1 (apply max (map first pos))))
  (define tiles-height (add1 (apply max (map second pos))))
  (define canvas (rectangle (offset-px tiles-width)
                            (offset-px tiles-height)
                            "solid" "transparent"))
  (define tile (block-tile col))
  (foldl (λ(cur-pos c) (place-image/align
             tile
             (offset-px (first cur-pos))
             (offset-px (second cur-pos)) "left" "top" c)) canvas pos))

(define (draw-board)
  (define board-width (* tile-size board-width-tiles))
  (define board-height (* tile-size board-height-tiles))
  (define offsets (stream-map #{* % tile-size} (in-naturals)))
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
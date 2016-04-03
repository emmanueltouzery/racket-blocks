#lang racket

(require 2htdp/universe 2htdp/image lang/posn)
(require picturing-programs)
(require match-plus)

(define tile-size 24)
(define tile-padding 1)

(define (make-gray x [a 255]) (make-color x x x a))

(define (gradient-rect color)
  (define (apply-factor x) (* x (quotient 255 tile-size)))
  (define base-rect (rectangle tile-size tile-size "solid" color))
  (overlay
   (map-image (λ(x y col) (make-gray (apply-factor x) 127)) base-rect)
   base-rect))

(define (block-tile color)
  (define bright-shade (make-gray 255 140))
  (define dark-shade (make-gray 0 140))
  (define overlay-width (quotient tile-size 7))
  (underlay (gradient-rect color)
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
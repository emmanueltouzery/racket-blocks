#lang racket

(require 2htdp/universe 2htdp/image lang/posn)
(require picturing-programs)

(define tile-size 16)

(define (make-gray x [a 255]) (make-color x x x a))

(define (make-gradient-rect color)
  (define (apply-factor x) (* x (quotient 255 tile-size)))
  (define base-rect (rectangle tile-size tile-size "solid" color))
  (overlay
   (map-image (λ(x y col) (make-gray (apply-factor x) 127)) base-rect)
   base-rect))

(define (make-tile color)
  (define bright-shade (make-gray 255 140))
  (define dark-shade (make-gray 0 140))
  (define overlay-width (quotient tile-size 5))
  (underlay (make-gradient-rect color)
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
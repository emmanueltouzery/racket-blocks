#lang curly-fn racket

(require 2htdp/universe 2htdp/image)
(require picturing-programs)
(require threading)

(define tile-size 16)

(define (make-gray x [a 255]) (make-color x x x a))

(define (make-gradient-rect color)
  (define apply-factor #{* % (quotient 255 tile-size)})
  (define base-rect (rectangle tile-size tile-size "solid" color))
  (overlay
   (map-image (λ(x y col) (make-gray (apply-factor x) 127)) base-rect)
   base-rect))

(define (make-tile color)
  (define shade-color (make-color 255 255 255 140))
  (define overlay-width (quotient tile-size 5))
  (define add-overlay
    #{place-image/align (rectangle %1 %2 "solid" shade-color) %3 %4 "left" "top" %5})
  (~> (make-gradient-rect color)
      (add-overlay overlay-width (- tile-size overlay-width) 0 0 _)
      (add-overlay (- tile-size (* 2 overlay-width)) overlay-width overlay-width 0 _)))
#lang racket

(require 2htdp/universe 2htdp/image)
(require picturing-programs)
(require threading)

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
  (define (add-overlay col w h x y base)
    (place-image/align (rectangle w h "solid" col) x y "left" "top" base))
  (~> (make-gradient-rect color)
      (add-overlay bright-shade overlay-width tile-size 0 0 _)
      (add-overlay bright-shade (- tile-size (* 2 overlay-width)) overlay-width overlay-width 0 _)
      (add-overlay dark-shade overlay-width (- tile-size (* 2 overlay-width)) (- tile-size overlay-width) overlay-width _)
      (add-overlay dark-shade tile-size overlay-width 0 (- tile-size overlay-width) _)))
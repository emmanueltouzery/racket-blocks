#lang racket

(require rackunit)

(require "../../drawing.rkt")
(require/expose "../../game-logic.rkt" (piece-depths cur-piece-state board-get-item))

(check-equal?
 (piece-depths (cur-piece-state (first pieces) #f 0 #f))
 #hash((0 . 1) (1 . 2) (2 . 1)))

(define test-board (list
    (list "red" "green" #f "blue")
    (list "yellow" #f #f "red" #f)))

(check-equal? (board-get-item empty 0 0) #f)
(check-equal? (board-get-item test-board 0 0) "yellow")
(check-equal? (board-get-item test-board 3 1) "blue")

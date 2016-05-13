#lang curly-fn racket

(require rackunit)

(require "../../drawing.rkt")
(require curly-fn)
(require/expose "../../game-logic.rkt"
                (piece-depths
                 cur-piece-state
                 board-get-item
                 game-state
                 piece-move-x
                 move-x-tolerance))

(check-equal?
 (piece-depths (cur-piece-state (first pieces) #f 0 #f))
 #hash((0 . 1) (1 . 2) (2 . 1)))

(define test-board (list
    (list "red" "green" #f "blue")
    (list "yellow" #f #f "red" #f)))

(check-equal? (board-get-item empty 0 0) #f)
(check-equal? (board-get-item test-board 0 0) "yellow")
(check-equal? (board-get-item test-board 3 1) "blue")

(define (get-piece-at x-tiles y-pixels)
  (cur-piece-state
    (piece "red" '((0 0)))
    (void)
    x-tiles y-pixels))

(define (test-game-state x-tiles y-pixels board)
  (game-state
       #f (get-piece-at x-tiles y-pixels)
       board (void)))

;; TODO add in a couple of good test cases
;; for moving left/right
(define (check-move-x msg in-game-state move expected-game-state)
  (check-equal?
   (piece-move-x move in-game-state)
   expected-game-state msg))

(define y-board-bottom
  (* (- tile-size 1) board-height-tiles))

(check-move-x "move left, empty board"
 (test-game-state 5 4 '())
 -1
 (test-game-state 4 4 '()))

(check-move-x "move left, already on the left edge"
 (test-game-state 0 4 '())
 -1
 (test-game-state 0 4 '()))

(let ([game-state
       (test-game-state 3 y-board-bottom '((#f #f "red" #f)))])
  (check-move-x "move left, cell occupied"
                game-state -1 game-state))

(let ([game-state
       (test-game-state 3 (- y-board-bottom move-x-tolerance) '((#f #f "red" #f)()))])
  (check-move-x "move left, cell occupied, not within tolerance to pass under it"
                game-state -1 game-state))

(let ([state-from-x
       #{test-game-state % (- y-board-bottom move-x-tolerance -1) '((#f #f "red" #f)())}])
  (check-move-x "move left, cell occupied but within tolerance to pass under it"
                (state-from-x 3) -1 (state-from-x 2)))
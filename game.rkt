#lang racket
(require "expander.rkt")

(define (play world)
  (displayln "Welcome to the dungeon!"))
  (define rooms (game-rooms world))
  (define start-room (hash-ref rooms 'wake))
  (displayln (room-v-name start-room))
  (displayln (room-v-desc start-room)))
  (for ([e (room-v-exits start-room)])
    (printf "Exit: ~a\n" (exit-v-direction e))))

(provide play)

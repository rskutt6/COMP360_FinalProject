#lang racket
(require "expander.rkt")

(define (play world)
  (displayln "Welcome to the dungeon!"))
  (define rooms (game-rooms world))
  (define start-room (hash-ref rooms 'wake))
  (displayln (room-v-name start-room))
  (displayln (room-v-desc start-room)))

(provide play)

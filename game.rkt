#lang racket

(struct game (rooms) #:transparent)
(struct room-v (name desc items monsters exits power) #:transparent)
(struct item-v (name desc type) #:transparent)
(struct monster-v (name hp) #:transparent)
(struct exit-v (direction destination) #:transparent)

(define (play world)
  (displayln "Welcome to the dungeon!")
  (define rooms (game-rooms world))
  (define start-room (hash-ref rooms "wake"))
  (displayln (room-v-name start-room))
  (displayln (room-v-desc start-room))
  (for ([e (room-v-exits start-room)])
    (printf "Exit: ~a\n" (exit-v-direction e))))

(provide (all-defined-out))
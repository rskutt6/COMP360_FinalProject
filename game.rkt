#lang racket
(struct game (rooms) #:transparent)
(struct room-v (name desc items monsters exits power) #:transparent)
(struct item-v (name desc type) #:transparent)
(struct monster-v (name hp) #:transparent)
(struct exit-v (direction destination) #:transparent)

(define (play world)
  (displayln "Welcome to the dungeon!")
  (define rooms (game-rooms world))
  (game-loop rooms "wake"))

(define (game-loop rooms current)
  (define r (hash-ref rooms current))
  
  ; print the current room
  (displayln (room-v-name r))
  (displayln (room-v-desc r))
  (for ([e (room-v-exits r)])
    (printf "Exit: ~a\n" (exit-v-direction e)))
  
  ; get player input
  (display "> ")
  (define input (read-line))
  
  ; check if they want to quit
  (cond
    [(equal? input "quit") (displayln "Goodbye!")]
    [else
     ; look through exits one by one
     (define next-room #f)
     (for ([e (room-v-exits r)])
       (when (equal? (exit-v-direction e) input)
         (set! next-room (exit-v-destination e))))
     
     ; did we find a matching exit?
     (if next-room
         (game-loop rooms next-room)
         (begin
           (displayln "Can't go that way.")
           (game-loop rooms current)))]))

(provide (all-defined-out))
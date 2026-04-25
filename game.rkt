#lang racket
(require racket/string)

; -----------------------------------------------
; STRUCTS
; define the data shapes for game world
; -----------------------------------------------

(struct game (rooms) #:transparent)
(struct room-v (name desc items monsters exits power) #:transparent)
(struct item-v (name desc type) #:transparent)
(struct monster-v (name hp) #:transparent)
(struct exit-v (direction destination) #:transparent)

; -----------------------------------------------
; PLAY
; entry point called by the expander
; -----------------------------------------------

(define (play world)
  (displayln "Welcome to the dungeon!")
  (define rooms (game-rooms world))
  (game-loop rooms "cave" 10))

; -----------------------------------------------
; GAME LOOP
; called every time the player enters a new room
; or after an action in the current room
; -----------------------------------------------

(define (game-loop rooms current player-power)
  (define r (hash-ref rooms current))

  ; ---- WIN CONDITION ----
  ; if the room has no exits, the player has reached the end
  (when (null? (room-v-exits r))
    (displayln (room-v-desc r))
    (displayln "You have reached the end... YOU WIN!")
    (exit))

  ; ---- PRINT ROOM INFO ----
  (printf "\nPower: ~a\n" player-power)
  (displayln (room-v-name r))
  (displayln (room-v-desc r))

  ; print exits
  (for ([e (room-v-exits r)])
    (printf "Exit: ~a\n" (exit-v-direction e)))

  ; print items
  (when (not (null? (room-v-items r)))
    (displayln "Items:")
    (for ([i (room-v-items r)])
      (printf "  ~a\n" (item-v-name i))))

  ; print monsters
  (when (not (null? (room-v-monsters r)))
    (displayln "Monsters:")
    (for ([m (room-v-monsters r)])
      (printf "  ~a (hp: ~a)\n" (monster-v-name m) (monster-v-hp m))))

  ; ---- ITEM PICKUP PHASE ----
  ; let player grab items before combat, returns updated power
  (define power-after-items
    (cond
      [(null? (room-v-items r)) player-power]
      [else
       (displayln "Type 'take <item>' to pick up an item, or 'continue' to proceed.")
       (display "> ")
       (define item-input (read-line))
       (printf "DEBUG item-input: '~a'\n" item-input)
       (printf "DEBUG char at 4: ~a\n" (char->integer (string-ref item-input 4)))
       (printf "DEBUG starts with take: ~a\n" (string-prefix? "take " item-input))
       (if (and (>= (string-length item-input) 5)
         (equal? (substring item-input 0 5) "take "))
           (let* ([item-name (substring item-input 5)]
                  [found-item (findf (lambda (i) (equal? (item-v-name i) item-name))
                                     (room-v-items r))])
             (printf "DEBUG item-name typed: ~a\n" item-name)
             (printf "DEBUG item-name in room: ~a\n" (item-v-name (car (room-v-items r))))
             (if found-item
                 (begin
                   (printf "You picked up ~a! Power +5\n" item-name)
                   (+ player-power 5))
                 (begin
                   (displayln "No such item here.")
                   player-power)))
           player-power)]))             ; continue with unchanged power

  (printf "DEBUG power-after-items: ~a\n" power-after-items)

  ; ---- COMBAT PHASE ----
  ; if there's a monster, player must fight or run
  ; uses power-after-items so pickup is reflected in combat
  (define power-after-combat
    (if (null? (room-v-monsters r))
        power-after-items  ; no monster, power unchanged
        (let ([m (car (room-v-monsters r))])
          (printf "A ~a blocks your path! (hp: ~a)\n" (monster-v-name m) (monster-v-hp m))
          (display "fight or run? > ")
          (define choice (read-line))
          (cond
            ; --- RUN ---
            ; show exits and let player flee
            [(equal? choice "run")
             (displayln "You run! Which way?")
             (for ([e (room-v-exits r)])
               (printf "Exit: ~a\n" (exit-v-direction e)))
             (display "> ")
             (define run-dir (read-line))
             ; find the matching exit
             (define run-dest #f)
             (for ([e (room-v-exits r)])
               (when (equal? (exit-v-direction e) run-dir)
                 (set! run-dest (exit-v-destination e))))
             ; go there or stay if invalid
             (if run-dest
                 (game-loop rooms run-dest power-after-items)
                 (begin
                   (displayln "Can't go that way!")
                   (game-loop rooms current power-after-items)))]

            ; --- FIGHT ---
            ; compare power to monster hp
            [(equal? choice "fight")
             (if (>= power-after-items (string->number (monster-v-hp m)))
                 (begin
                   (displayln "You defeated the monster! Power +5")
                   (+ power-after-items 5))  ; return new power
                 (begin
                   (displayln "You are too weak... YOU DIED!")
                   (exit)))]

            ; anything else, stay and try again
            [else
             (displayln "Type 'fight' or 'run'.")
             (game-loop rooms current power-after-items)]))))

  ; ---- MOVEMENT PHASE ----
  ; let the player move to another room
  (display "> ")
  (define input (read-line))

  (cond
    ; quit the game
    [(equal? input "quit")
     (displayln "Goodbye!")]

    ; move to another room
    [else
     (define next-room #f)
     ; look through exits to find a match
     (for ([e (room-v-exits r)])
       (when (equal? (exit-v-direction e) input)
         (set! next-room (exit-v-destination e))))
     ; go there or stay if invalid
     (if next-room
         (game-loop rooms next-room power-after-combat)
         (begin
           (displayln "Can't go that way.")
           (game-loop rooms current power-after-combat)))]))

(provide (all-defined-out))
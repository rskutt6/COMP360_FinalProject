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

(define (get-room rooms name)
  (cdr (assoc name rooms)))

; -----------------------------------------------
; PLAY
; entry point called by the expander
; -----------------------------------------------

(define (play world)
  (displayln "==========================================")
  (displayln "       WELCOME TO THE DUNGEON!")
  (displayln "==========================================")
  (define rooms (game-rooms world))
  (define start-name (car (car rooms)))
  (game-loop rooms start-name 10))

; -----------------------------------------------
; GAME LOOP
; called when player enters new room or after an action
; -----------------------------------------------

(define (game-loop rooms current player-power)
  (define r (get-room rooms current))

  ; ---- WIN CONDITION ----
  (when (null? (room-v-exits r))
    (displayln "==========================================")
    (displayln (room-v-desc r))
    (displayln "==========================================")
    (displayln "          ★ YOU WIN! ★")
    (printf "    You escaped with a power of ~a!\n" player-power)
    (displayln "==========================================")
    (exit))

  ; ---- PRINT ROOM INFO ----
  (displayln "==========================================")
  (printf "  ~a~aPower: ~a\n"
          (string-upcase (room-v-name r))
          (make-string (max 1 (- 28 (string-length (room-v-name r)))) #\space)
          player-power)
  (displayln "==========================================")
  (printf "  ~a\n\n" (room-v-desc r))
  (printf "  Exits:    ~a\n" (string-join (map exit-v-direction (room-v-exits r)) " | "))
  (when (not (null? (room-v-items r)))
    (printf "  Items:    ~a\n" (string-join (map item-v-name (room-v-items r)) ", ")))
  (when (not (null? (room-v-monsters r)))
    (printf "  Monsters: ~a\n" (string-join (map (lambda (m) (format "~a (hp: ~a)" (monster-v-name m) (monster-v-hp m)))
                                                 (room-v-monsters r)) ", ")))
  (displayln "==========================================")



  ; ---- ITEM PICKUP PHASE ----
  ; grab items before combat & return updated power
  (define power-after-items
    (cond
      [(null? (room-v-items r)) player-power]
      [else
       (displayln "Type 'take <item>' to pick up an item, 'continue' to proceed, or 'quit' to exit.")
       (display "> ")
       (define item-input (read-line))
       (when (equal? item-input "quit") (displayln "Goodbye!") (exit))
       (if (and (>= (string-length item-input) 5)
         (equal? (substring item-input 0 5) "take "))
           (let* ([item-name (substring item-input 5)]
                  [found-item (findf (lambda (i) (equal? (item-v-name i) item-name))
                                     (room-v-items r))])
             (if found-item
                 (begin
                   (printf "You picked up ~a! Power +5\n" item-name)
                   (+ player-power 5))
                 (begin
                   (displayln "No such item here.")
                   player-power)))
           player-power)]))             

  ; ---- COMBAT PHASE ----
  ; if there's a monster, player must fight or run
  
  (define power-after-combat
    (if (null? (room-v-monsters r))
        power-after-items  
        (let ([m (car (room-v-monsters r))])
          (printf "A ~a blocks your path! (hp: ~a)\n" (monster-v-name m) (monster-v-hp m))
          (display "fight or run? > ")
          (define choice (read-line))
          (when (equal? choice "quit") (displayln "Goodbye!") (exit))
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
             ; exit or stay if invalid
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
  (displayln "Where do you go?")
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
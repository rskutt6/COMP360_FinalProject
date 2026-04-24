#lang racket
(require "expander.rkt")

(define (play-game world starting-room)
  (let loop ([current-name starting-room])
    (define room (cdr (assoc current-name (map (lambda (p) p) (hash->list (game-rooms world))))))
    
    ;; print room info
    (printf "\n=== ~a ===\n" (room-v-name room))
    (printf "~a\n" (room-v-desc room))
    
    ;; print items
    (when (not (null? (room-v-items room)))
      (displayln "Items:")
      (for ([i (room-v-items room)])
        (printf "  - ~a: ~a\n" (item-v-name i) (item-v-desc i))))
    
    ;; print monsters
    (when (not (null? (room-v-monsters room)))
      (displayln "Monsters:")
      (for ([m (room-v-monsters room)])
        (printf "  - ~a (hp: ~a)\n" (monster-v-name m) (monster-v-hp m))))
    
    ;; print exits
    (displayln "Exits:")
    (for ([e (room-v-exits room)])
      (printf "  - ~a\n" (exit-v-direction e)))
    
    ;; get input
    (display "\n> ")
    (define input (string-trim (read-line)))
    
    ;; find matching exit
    (define next
      (for/first ([e (room-v-exits room)]
                  #:when (string=? (exit-v-direction e) input))
        (exit-v-destination e)))
    
    (cond
      [(equal? input "quit") (displayln "Goodbye!")]
      [next (loop next)]
      [else (displayln "No exit that way.") (loop current-name)])))

(provide play-game)
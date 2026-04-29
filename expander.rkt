#lang br/quicklang

(require racket/match
         brag/support
         "game.rkt")

;; the whole parsed program will first be handed to dungeon-module-begin

(provide (rename-out [dungeon-module-begin #%module-begin]))
(provide (matching-identifiers-out
          #rx"^(program|room|room-body|room-element|desc|item|item-body|item-field|type|monster|exit|power|key)$"
          (all-defined-out)))

;; runtime data

;(struct game (rooms) #:transparent)
;(struct room-v (name desc items monsters exits power) #:transparent)
;(struct item-v (name desc type) #:transparent)
;(struct monster-v (name hp) #:transparent)
;(struct exit-v (direction destination key) #:transparent)

;; small wrapper values used while assembling room and items.

(struct desc-node (text) #:transparent)
(struct item-node (value) #:transparent)
(struct monster-node (value) #:transparent)
(struct exit-node (value) #:transparent)
(struct power-node (value) #:transparent)
(struct type-node (value) #:transparent)
(struct key-node (value) #:transparent)
(struct health-node (value) #:transparent)

;; HELPER: remove surrounding quotes from strings

(define (unquote s)
  (if (and (string? s)
           (>= (string-length s) 2)
           (string=? (substring s 0 1) "\"") ; first char is "
           (string=? (substring s (sub1 (string-length s))) "\"")) ; last char is "
      ;; strip off the first and last character
      (substring s 1 (sub1 (string-length s)))
      ;; otherwise leave unchanged
      s))

;; BUILD AN ITEM FROM ITS FIELDS

;; function walks over fields, picks out the desc and type, and produces one final item-v stuct
(define (build-item name fields)
  ;; start with not found yet values
  (define desc-text #f)
  (define item-type #f)

  ;; loop over each field inside the item
  (for ([f fields])
    (match f
      ;; if field = desc node, extract text, remove quotes and store it
      [(desc-node txt)
       (set! desc-text (unquote txt))]

      ;; if the field is a type node, store the type
      [(type-node t)
       (set! item-type t)]

      ;; any other field is invalid
      [_ (error 'build-item "invalid item field: ~a" f)]))

  ;; return final item value
  (item-v name desc-text item-type))

;; BUILD A ROOM FROM ITS ELEMENTS

(define (build-room name elements)
  ;; fields we expect to accumulate
  (define desc-text #f)
  (define items '())
  (define monsters '())
  (define exits '())
  (define room-power #f)

  ;; walk through each element inside the room
  (for ([el elements])
    (match el
      ;; Room desc
      [(desc-node txt)
       (set! desc-text (unquote txt))]

      ;; Item element
      [(item-node i)
       (set! items (cons i items))]

      ;; Monster element
      [(monster-node m)
       (set! monsters (cons m monsters))]

      ;; Exit element
      [(exit-node e)
       (set! exits (cons e exits))]

      ;; Power element
      [(power-node p)
       (set! room-power p)]

      ;; anything else not allowed
      [_ (error 'build-room "invalid room element: ~a" el)]))

  ;; reverse restores the source order (because list was built with cons)
  (room-v name
          desc-text
          (reverse items)
          (reverse monsters)
          (reverse exits)
          room-power))

;; PARSE_TREE EXPANDERS

;; ---room---
;; each room will expand into a definition

(define-macro (room NAME BODY)
  (with-pattern ([ROOM-ID (prefix-id "room-" #'NAME #:source #'NAME)])
    (syntax/loc caller-stx
      (define ROOM-ID
        (build-room NAME BODY)))))

(define-macro (room-body ELEMENT ...)
  #'(list ELEMENT ...))

(define-macro (room-element ELEMENT)
  #'ELEMENT)

(define-macro (item-body FIELD ...)
  #'(list FIELD ...))

(define-macro (item-field FIELD)
  #'FIELD)


;; ---desc---

(define-macro (desc STR)
  #'(desc-node STR))

;; ---item---

(define-macro (item NAME BODY)
  #'(item-node (build-item NAME BODY)))

;; ---type---

(define-macro (type NAME)
  #'(type-node NAME))
;; ---key---
(define-macro (key NAME)
  #'(key-node NAME))

;; ---monster---

(define-macro (monster NAME HP)
  #'(monster-node (monster-v NAME HP)))

;; ---exit---

(define-macro (exit DIR DEST)
  #'(exit-node (exit-v DIR DEST)))

;; ---power---

(define-macro (power N)
  #'(power-node N))

;; ---health---
(define-macro (health N)
  #'(health-node N))

;; -----dungeon-module-begin-----
;; macro for the whole program

(define-macro (dungeon-module-begin (program STUFF ...))
  (let ([health 100]  ; default
        [rooms '()])
    (for ([s (syntax->list #'(STUFF ...))])
      (syntax-case s (health room)
        [(health N) (set! health (syntax->datum #'N))]
        [(room . _) (set! rooms (cons s rooms))]
        [_ (error "Invalid top-level form")]))
    (with-syntax ([HEALTH health]
                  [(ROOM ...) (reverse rooms)]
                  [(ROOM-NAMES ...) (map (lambda (r) (datum->syntax #f (cadr (syntax->list r)))) rooms)]
                  [(ROOM-ID ...) (prefix-id "room-" (map (lambda (r) (cadr (syntax->list r))) rooms))])
      #'(#%module-begin
         ROOM ...
         (define game-world (game (list (cons ROOM-NAMES ROOM-ID) ...) HEALTH))
         (play game-world)
         (provide game-world)))))





































  
           

  
  
  

  
  
    

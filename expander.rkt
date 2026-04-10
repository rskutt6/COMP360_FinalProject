#lang racket

(require racket/match)

;; expand-program = takes parse tree and prints result
(provide expand-program)

(define (expand-program parse-tree)
  (define tree
    (if (syntax? parse-tree)
    (syntax->datum parse-tree)
    parse-tree))

  ;; printed header
  (displayln " Expanded Adventure ")
  ;; walk through tree
  (expand-node tree))

(define (expand-node node)
  (cond
    [(not (list? node)) (void)] ;; ignore if not a list
    [(and (pair? node) (eq? (first node) 'program))
     (for-each expand-node (rest node))]
    ;; if a room is found
    [(and (pair? node) (eq? (first node) 'room))
     (expand-room node)]
    ;; keep searching
    [else
     (for-each expand-node (rest node))]))

;; Room Expansion
(define (expand-room node)
  (define room-name (find-first 'ID node))
  ;; print room name
  (printf "Room: ~a\n" room-name)
  (define desc (find-desc node))
  (when desc
    (printf " Description: ~a\n" desc))

  ;; Items - find all item nodes inside the room
  (for ([item (find-all-subrules 'item node)])
    (define item-name (find-first 'ID item))
    (printf " Item: ~a\n" item-name)
    ;; item description
    (define item-desc (find-desc item))
    (when item-desc
      (printf " Desc: ~a\n" item-desc))
    ;; item type
    (define item-type (find-type item))
    (when item-type
      (printf " Type: ~a\n" item-type)))

  ;; Monsters - find all monster nodes
  (for ([monster (find-all-subrules 'monster node)])
    (define vals (find-direct-values monster '(ID NUMBER)))
    (when (=(length vals) 2)
      (printf " Monster: ~a (power ~a)\n"
              (first vals)
              (second vals))))

  ;; Exits - find all exit nodes
  (for ([exit (find-all-subrules 'exit node)])
    (define vals (find-direct-values exit '(ID ID)))
    (when (= (length vals) 2)
      (printf " Exit: ~a -> ~a\n"
              (first vals)
              (second vals)))))

;; Helper Functions

;; search the tree for the first occurence of a token type
(define (find-first token-name node)
  (cond
    [(not (list? node)) #f] ;; if not a list there is nothing to search
    [(and (= (length node) 2) ;; if node is a token
          (eq? (first node) token-name))
     (second node)]
    [else
     (for/first ([child node]
                 #:when (find-first token-name child))
       (find-first token-name child))]))

;; find all subtrees that match a rule name
(define (find-all-subrules rule-name node)
  (cond
    [(not (list? node)) '()]
    [else
     (append
      (if (and (pair? node) (eq? (first node) rule-name))
          (list node)
          '())
      ;; search children
      (append-map (lambda (child)
                    (find-all subrules rule-name child))
                  (rest node)))]))

;; find-desc
(define (find-desc node)
  (define desc-node
    (for/first ([sub (find-all-subrules 'desc node)])
      sub))
  (and desc-node
       (strip-quotes (find-first 'STRING desc-node))))

;; find-type
(define (find-type node)
  (define type node
    (for/first ([sub (find-all-subrules 'type node)])
      sub))
  (and type-node
       (find-first 'ID type-node)))

;; find-number-in-power
(define (find-number-in-power node)
  (define power-node
    (for/first ([sub (find-all-subrules 'power node)])
      sub))
  (and power-node
       (find-first 'NUMBER power-node)))

;; find-direct-values
(define (find-direct-values node expected-kinds)
  (define children (rest node))
  (for/list ([child children]
             #:when (and (list? child)
                         (= (length child) 2)
                         (symbol? (first child))))
    (second child)))

;; strip-quotes
(define (strip-quotes s)
  (if (and (string? s)
           (>= (string-length s) 2)
           (equal? (substring s 0 1) "\"")
           (equal? (substring s (- (string-length s) 1)) "\""))
      (substring s 1 (- (string-length s) 1))
      s))

  
  
  

  
  
    

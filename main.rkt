#lang racket

;; import your components
(require "lexer.rkt"
         "parser.rkt"     ;; generated from your brag file
         "expander.rkt")

;; =========================
;; Runs your full pipeline
;; =========================

(define (run-file path)
  ;; open the input file
  (define in (open-input-file path))

  ;; run parser using your tokenizer
  (define parse-tree
    (parse path (make-tokenizer in)))

  ;; OPTIONAL: print parse tree for debugging
  ;; (pretty-print (syntax->datum parse-tree))

  ;; run your expander
  (expand-program parse-tree)

  ;; close file
  (close-input-port in))

;; =========================
;; Run automatically
;; =========================

(module+ main
  (displayln "--- test.txt ---")
  (run-file "test.txt")
  (displayln "--- test2.txt ---")
  (run-file "test2.txt"))
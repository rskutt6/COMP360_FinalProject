#lang racket

(require "parser.rkt"
         "lexer.rkt")

(define in (open-input-file "test.txt"))

(define parse-tree
  (parse "test.txt" (make-tokenizer in)))

(pretty-print (syntax->datum parse-tree))

(close-input-port in)

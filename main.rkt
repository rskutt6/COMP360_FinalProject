#lang br/quicklang
(require "parser.rkt" "lexer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (strip-bindings
   #`(module basic-parser-mod COMP360_FinalProject/expander
       #,parse-tree)))

(module+ reader
  (provide read-syntax))

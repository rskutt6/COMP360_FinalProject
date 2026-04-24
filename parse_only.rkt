#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")

(define (read-syntax path port)
  (displayln "are we here")
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-bindings
   #`(module basic-parser-mod COMP360_FinalProject/parse_only
       #,parse-tree)))

(module+ reader (provide read-syntax))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE))

(provide (rename-out [parser-only-mb #%module-begin]))

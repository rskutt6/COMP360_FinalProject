#lang br
(require "lexer.rkt" brag/support)

(define (make-tokenizer ip [path #f]) ; has an optional second argument, path
  (port-count-lines! ip)
  (lexer-file-path path) ; let's us tell the lexer where the source is, for source locations!
  (define (next-token) (basic-lexer ip))
  next-token)

(provide make-tokenizer)

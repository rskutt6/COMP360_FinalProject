#lang racket

(require brag/support)
(provide make-tokenizer)

(define (make-tokenizer port)
  (define (next-token)
    (define dungeon-lexer
      (lexer

       ;keywords
       ["room"     lexeme]
       ["desc"     lexeme]
       ["item"     lexeme]
       ["exit"     lexeme]
       ["monster"  lexeme]
       ["type"     lexeme]
       ["power"    lexeme]

       ;numbers
       [(repetition 1 +inf.0 (char-range "0" "9")) (token 'NUMBER lexeme)]

       ;identifiers
       [(concatenation
         (union (char-range "a" "z") (char-range "A" "Z"))
         (repetition 0 +inf.0 (union (char-range "a" "z")
                                     (char-range "A" "Z")
                                     (char-range "0" "9")
                                     (char-set "-_"))))
        (token 'ID lexeme)]

       [(concatenation "\""
                       (repetition 0 +inf.0 (char-complement (char-set "\"")))
                       "\"")
        (token 'STRING lexeme)]

       ;punctuation
       ["(" (token 'LPAREN lexeme)]
       [")" (token 'RPAREN lexeme)]
       [":" (token 'COLON lexeme)]
       
       ; provided:
       [whitespace (next-token)] ; skip whitespace
       [any-char (next-token)]   ; ignore unrecognized characters
       [(eof) eof]))             ; end of file

    (dungeon-lexer port))
  next-token)
#lang racket

(require brag/support)

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
       [whitespace (token lexeme #:skip? #t)] ; skip whitespace
       [any-char (token lexeme #:skip? #t)]   ; ignore unrecognized characters
       [(eof) eof]))             ; end of file

(provide dungeon-lexer)
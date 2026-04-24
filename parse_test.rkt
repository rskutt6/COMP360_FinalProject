#lang br
(require basic/parser basic/tokenizer brag/support)

; #<<HERE acts like a large block string, to capture the entire newline-separated string we define
(define str #<<HERE
10 print "hello" ; "world"
20 goto 9 + 10 + 11
30 end
HERE
)

(parse-to-datum (apply-tokenizer make-tokenizer str))

#lang racket

(provide (all-defined-out))

#| board turn en_passant castle |#
(struct node (b t e c))

(define i_b
  (string-append
                "         \n" 
                "         \n" 
                " rnbqkbnr\n" 
                " pppppppp\n" 
                " ........\n" 
                " ........\n" 
                " ........\n" 
                " ........\n" 
                " PPPPPPPP\n" 
                " RNBQKBNR\n" 
                "         \n" 
                "         \n"))

(define f_c '("wk" "wq" "bk" "bq"))




#lang racket

(provide (except-out (all-defined-out) root))

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

(define root
  (node i_b #t 0 '()))

(define (gen n x)
  (case (string-ref (node-b n) x)
    [(#\P) (- x 10)]
    [else "nothing"]
  ))






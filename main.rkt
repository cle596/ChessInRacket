#lang racket

(require "node.rkt"
         "util.rkt"
         "input.rkt"
         "search.rkt")

(define root
  (node i_b #t 0 '("wk" "wq" "bk" "bq") (cons 0 0) 0))

(define (loop n)
  (begin
    (displayln (pretty n))
    (let ([i (update n (in (read-line)))])
      (loop (update i (car (spawn 8 i)))))))

;(loop root)

(define (pront x)
  (displayln x)
  (flush-output))

(define (writ x)
  (call-with-output-file "/home/ch/Documents/racketess/x.txt"
    (lambda (out)
      (display (string-append x (string #\newline)) out)) #:mode 'text #:exists 'append))
;(open-output-file "./x.txt" #:mode 'text #:exists 'append)

(define (gui n)
  (let ([i (read-line)])
    (writ i)
    (case i
      [("xboard") (gui n)]
      [("protover 2")
       (begin
         (pront (string-append
                 "feature "
                 ;"usermove=1 "
                 "setboard=0 "
                 "sigint=0 "
                 "variants='normal' "
                 "done=1"))
         (gui n))]
      [("go")
       (let ([m (ttrans (car (spawn 8 n)))])
         (pront (string-append "move " m))
         (gui-loop (update n (rtrans m)) "white"))]
      ;[("usermove") (pront (string-append "move " (car (spawn 8 n))))]
      [else (if (regexp-match #rx"[a-h][1-8][a-h][1-8]" i)
                (gui-loop (update n (rtrans i)) "black")
                (void)
                )]
      ))
  (gui n))

(define (gui-loop n c)
  (if (equal? c "white")
      (let ([i (read-line)])
        (if (regexp-match #rx"[a-h][1-8][a-h][1-8]" i)
            (gui-loop (update n (rtrans i)) "black")
            (gui-loop n "white")))
      (let ([x (ttrans (car (spawn 8 n)))])
        (pront (string-append "move " x))
        (gui-loop (update n (rtrans x)) "white")
        )
      )
  )

(gui root)
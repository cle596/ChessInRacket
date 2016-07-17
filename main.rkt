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

(loop root)

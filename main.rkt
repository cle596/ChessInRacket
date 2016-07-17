#lang racket

(require "node.rkt"
         "util.rkt"
         "input.rkt")

(define root
  (node i_b #t 0 '("wk" "wq" "bk" "bq") (cons 0 0) 0))

(define (loop n)
  (begin
    (displayln (pretty n))
    ;(displayln (node-t n))
    ;(displayln (node-c n))
    (displayln (map ni (gen_all n)))
      (loop (update n (in (read-line))))))

(loop root)

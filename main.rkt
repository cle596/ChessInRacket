#lang racket

(require "node.rkt"
         "util.rkt"
         "input.rkt")

(define root
  (node i_b #t 0 '()))

(define (loop n)
  (begin
    (displayln (pretty n))
    (let ([m (read-line)])
      (loop (update n (in m))))))

(loop root)

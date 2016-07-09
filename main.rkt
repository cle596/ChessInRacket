#lang racket

(require "node.rkt" "util.rkt")

(define root
  (node i_b #t 0 '()))

(displayln (pretty root))
;read-line)
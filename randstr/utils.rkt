#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random)

(provide
 (contract-out
  [remove-duplicates-preserving-order ((listof any/c) . -> . (listof any/c))]))

;; Remove duplicates while preserving order
(define (remove-duplicates-preserving-order lst)
  (let loop ([lst lst]
             [seen '()]
             [result '()])
    (cond
      [(null? lst) (reverse result)]
      [(member (car lst) seen)
       (loop (cdr lst) seen result)]
      [else
       (loop (cdr lst) (cons (car lst) seen) (cons (car lst) result))])))
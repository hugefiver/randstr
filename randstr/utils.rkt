#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random)

(provide
 (contract-out
  [remove-duplicates-preserving-order ((listof any/c) . -> . (listof any/c))]))

;; Remove duplicates while preserving order
;; Uses a mutable hash table for ~O(1) average membership checks.
;; For very small lists, the overhead vs. `member` may not be noticeable.
(define (remove-duplicates-preserving-order lst)
  (define seen (make-hash))
  (let loop ([lst lst]
             [result '()])
    (cond
      [(null? lst) (reverse result)]
      [else
       (define x (car lst))
       (if (hash-has-key? seen x)
           (loop (cdr lst) result)
           (begin
             (hash-set! seen x #t)
             (loop (cdr lst) (cons x result))))])))
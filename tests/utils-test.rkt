#lang racket/base

(require rackunit
         "../randstr/utils.rkt")

;; Test cases for utils module

(test-case "remove-duplicates-preserving-order: removes duplicates"
  (let ([result (remove-duplicates-preserving-order '(a b c b a d))])
    (check-equal? result '(a b c d))))

(test-case "remove-duplicates-preserving-order: preserves order"
  (let ([result (remove-duplicates-preserving-order '(z x y x w z))])
    (check-equal? result '(z x y w))))

(test-case "remove-duplicates-preserving-order: handles empty list"
  (let ([result (remove-duplicates-preserving-order '())])
    (check-equal? result '())))

(test-case "remove-duplicates-preserving-order: handles single element"
  (let ([result (remove-duplicates-preserving-order '(a))])
    (check-equal? result '(a))))

(test-case "remove-duplicates-preserving-order: no duplicates"
  (let ([result (remove-duplicates-preserving-order '(a b c d))])
    (check-equal? result '(a b c d))))

(test-case "remove-duplicates-preserving-order: all same elements"
  (let ([result (remove-duplicates-preserving-order '(x x x x))])
    (check-equal? result '(x))))

;; Run tests
(void)
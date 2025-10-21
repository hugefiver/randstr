#lang racket/base

(require rackunit
         "../main.rkt"
         "./test-helpers.rkt")

(define (check pattern)
  ((make-pattern-checker pattern)))

;; Test that the optimized implementation produces the same results as expected

(test-case "randstr basic functionality"
  (check-true (check "abc"))
  
  (let ([result (randstr "[a-z]{3}")])
    (check-equal? (string-length result) 3)
    (check-true (check "[a-z]{3}"))
    (for ([char (string->list result)])
      (check-true (char>=? char #\a))
      (check-true (char<=? char #\z))))
 
  (let ([results (randstr* "[0-9]{2}" 5)])
    (check-equal? (length results) 5)
    (for ([result results])
      (check-equal? (string-length result) 2)
      (check-true (check "[0-9]{2}"))
      (for ([char (string->list result)])
        (check-true (char>=? char #\0))
        (check-true (char<=? char #\9))))))

(test-case "character class with quantifier"
  (let ([result (randstr "[a-z]{5}")])
    (check-equal? (string-length result) 5)
    (check-true (check "[a-z]{5}"))))

;; Run all tests
(void)
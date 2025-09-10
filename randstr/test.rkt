#lang racket/base

(require rackunit
         "main.rkt")

;; Tests for the randstr library
(check-true (string? (randstr "abc")))
(check-equal? (length (randstr* "abc" 5)) 5)
(check-true (andmap string? (randstr* "test" 3)))

(printf "All tests passed!\n")
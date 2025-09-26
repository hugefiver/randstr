#lang racket/base

(require rackunit
         "main.rkt")

;; Tests for the randstr library
(check-true (string? (randstr "abc")))
(check-equal? (length (randstr* "abc" 5)) 5)
(check-true (andmap string? (randstr* "test" 3)))

;; Tests for new POSIX extensions
(check-true (string? (randstr "\\w+")))
(check-true (string? (randstr "\\W+")))
(check-true (string? (randstr "\\s*")))
(check-true (string? (randstr "\\S+")))
(check-true (string? (randstr "\\d+")))
(check-true (string? (randstr "\\D+")))

;; Tests for POSIX character classes
(check-true (string? (randstr "[[:alpha:]]+")))
(check-true (string? (randstr "[[:digit:]]+")))
(check-true (string? (randstr "[[:alphanum:]]+")))
(check-true (string? (randstr "[[:alnum:]]+")))
(check-true (string? (randstr "[[:word:]]+")))
(check-true (string? (randstr "[[:blank:]]*")))
(check-true (string? (randstr "[[:space:]]+")))
(check-true (string? (randstr "[[:upper:]]+")))
(check-true (string? (randstr "[[:lower:]]+")))
(check-true (string? (randstr "[[:ascii:]]+")))
(check-true (string? (randstr "[[:cntrl:]]*")))
(check-true (string? (randstr "[[:graph:]]+")))
(check-true (string? (randstr "[[:print:]]+")))
(check-true (string? (randstr "[[:punct:]]+")))
(check-true (string? (randstr "[[:xdigit:]]+")))

;; Tests for nested POSIX character classes
(check-true (string? (randstr "[[:upper:]0-9]+")))
(check-true (string? (randstr "[[:lower:]_]+")))
(check-true (string? (randstr "[[:digit:]a-c]+")))
(check-true (string? (randstr "[[:alpha:]0-9]+")))

;; Tests for character class duplicate element handling
;; Simple duplicate characters
(check-true (string? (randstr "[aaabbbccc]")))
;; Overlapping ranges
(check-true (string? (randstr "[a-cb-d]")))
;; Mixed characters and ranges
(check-true (string? (randstr "[ab-d123]")))
;; POSIX character classes with duplicates
(check-true (string? (randstr "[[:digit:]0-2]")))
;; Complex case with multiple duplicates
(check-true (string? (randstr "[aaabbbccc[:lower:]d-e]")))

(printf "All tests passed!\n")
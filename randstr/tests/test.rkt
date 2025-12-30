#lang racket/base

(require rackunit
         racket/string
         "../main.rkt")

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

;; parse-character-class should return a vector of unique options
(let-values ([(options remaining) (parse-character-class (string->list "ab]"))])
  (check-true (vector? options))
  (let ([options-list (vector->list options)])
    (check-not-false (member #\a options-list))
    (check-not-false (member #\b options-list)))
  (check-equal? remaining '()))

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

;; Tests for normal distribution quantifiers
;; {n+} - 2nd order normal distribution
(check-true (string? (randstr "\\w{5+}")))
(check-true (string? (randstr "[a-z]{10+}")))
;; {n++} - 3rd order normal distribution
(check-true (string? (randstr "\\d{8++}")))
;; {n+++} - 4th order normal distribution (more concentrated)
(check-true (string? (randstr "[A-Z]{15+++}")))

;; Tests for range normal distribution quantifiers
;; {n1+n2} - range 2nd order normal distribution
(let ([result (randstr "\\w{5+10}")])
  (check-true (string? result))
  (check-true (<= 5 (string-length result) 10)))
;; {n1++n2} - range 3rd order normal distribution
(let ([result (randstr "[a-z]{3++8}")])
  (check-true (string? result))
  (check-true (<= 3 (string-length result) 8)))
;; {+n} - shorthand for {0+n}
(let ([result (randstr "\\d{+5}")])
  (check-true (string? result))
  (check-true (<= 0 (string-length result) 5)))
;; {++n} - shorthand for {0++n}
(let ([result (randstr "[A-Z]{++10}")])
  (check-true (string? result))
  (check-true (<= 0 (string-length result) 10)))

;; Tests for named groups and backreferences
;; Basic named group definition
(check-true (string? (randstr "(?<name>\\w{3})")))
;; Named group with backreference
(let ([result (randstr "(?<word>[a-z]{4})-\\k<word>")])
  (check-true (string? result))
  ;; Check that the pattern produces correct format (word-word where both words are same)
  (let ([parts (string-split result "-")])
    (check-equal? (length parts) 2)
    (check-equal? (car parts) (cadr parts))))
;; Multiple backreferences
(let ([result (randstr "(?<a>\\d{2})(?<b>[a-z]{2}):\\k<a>-\\k<b>")])
  (check-true (string? result)))
;; Named group with quantifier on backreference
(check-true (string? (randstr "(?<prefix>[A-Z]{2})\\k<prefix>{2}")))

(printf "All tests passed!\n")

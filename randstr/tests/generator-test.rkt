#lang racket/base

(require rackunit
         "../generator.rkt"
         "../tokenizer.rkt"
         "./test-helpers.rkt")

(define (check pattern)
  ((make-pattern-checker pattern)))

;; Test cases for generator module
(test-case "generate-from-tokens: literal characters"
  (check-true (check "abc")))

(test-case "generate-from-tokens: literal characters with anchors"
  (check-true (check "^abc$")))

(test-case "generate-from-tokens: character class"
  (check-true (check "[abc]")))

(test-case "generate-from-tokens: escape sequences"
  (check-true (check "\\d\\w\\s")))

(test-case "generate-from-tokens: quantifiers"
  (check-true (check "a{3}")))

(test-case "generate-from-tokens: star quantifier"
  (check-true (check "a*")))

(test-case "generate-from-tokens: plus quantifier"
  (check-true (check "b+")))

(test-case "generate-from-tokens: optional quantifier"
  (check-true (check "c?")))

;; Run tests
(void)

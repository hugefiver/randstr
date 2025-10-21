#lang racket/base

(require rackunit
         "../generator.rkt"
         "../tokenizer.rkt")

;; Test cases for generator module
(test-case "generate-from-tokens: literal characters"
  (let ([tokens (tokenize-pattern "abc")])
    (let ([result (generate-from-tokens tokens)])
      (check-equal? result "abc"))))

(test-case "generate-from-tokens: character class"
  (let ([tokens (tokenize-pattern "[abc]")])
    (let ([result (generate-from-tokens tokens)])
      (check-equal? (string-length result) 1)
      (check-true (or (string=? result "a")
                      (string=? result "b")
                      (string=? result "c"))))))

(test-case "generate-from-tokens: escape sequences"
  (let ([tokens (tokenize-pattern "\\d\\w\\s")])
    (let ([result (generate-from-tokens tokens)])
      (check-equal? (string-length result) 3)
      (check-true (char-numeric? (string-ref result 0)))
      ;; We can't easily test \w and \s without more specific checks
      )))

(test-case "generate-from-tokens: quantifiers"
  (let ([tokens (tokenize-pattern "a{3}")])
    (let ([result (generate-from-tokens tokens)])
      (check-equal? result "aaa"))))

(test-case "generate-from-tokens: star quantifier"
  (let ([tokens (tokenize-pattern "a*")])
    (let ([result (generate-from-tokens tokens)])
      (check-true (string? result))
      (check-true (<= (string-length result) 4)) ; random 0-4 chars
      (for ([c (string->list result)])
        (check-equal? c #\a)))))

(test-case "generate-from-tokens: plus quantifier"
  (let ([tokens (tokenize-pattern "b+")])
    (let ([result (generate-from-tokens tokens)])
      (check-true (string? result))
      (check-true (>= (string-length result) 1)) ; at least 1 char
      (check-true (<= (string-length result) 5)) ; random 1-5 chars
      (for ([c (string->list result)])
        (check-equal? c #\b)))))

(test-case "generate-from-tokens: optional quantifier"
  (let ([tokens (tokenize-pattern "c?")])
    (let ([result (generate-from-tokens tokens)])
      (check-true (or (string=? result "") (string=? result "c"))))))

;; Run tests
(void)
#lang racket/base

(require rackunit
         "../randstr/char-classes.rkt")

;; Test cases for char-classes module

(test-case "random-character: generates valid character"
  (let ([char (random-character)])
    (check-true (char? char))
    (check-true (or (char-alphabetic? char)
                    (char-numeric? char)))))

(test-case "random-word-char: generates valid word character"
  (let ([char (random-word-char)])
    (check-true (char? char))
    (check-true (or (char-alphabetic? char)
                    (char-numeric? char)
                    (char=? char #\_)))))

(test-case "random-whitespace-char: generates valid whitespace character"
  (let ([char (random-whitespace-char)])
    (check-true (char? char))
    (check-true (or (char=? char #\space)
                    (char=? char #\tab)
                    (char=? char #\newline)
                    (char=? char #\return)))))

(test-case "random-digit-char: generates valid digit character"
  (let ([char (random-digit-char)])
    (check-true (char? char))
    (check-true (char-numeric? char))))

(test-case "random-ref: returns element from list"
  (let ([lst '(a b c d e)])
    (let ([result (random-ref lst)])
      (check-not-false (member result lst)))))

(test-case "character lists: contain expected characters"
  (let ([alpha-chars (alphabetic-chars)])
    (check-not-false (member #\a alpha-chars))
    (check-not-false (member #\Z alpha-chars)))
  (let ([num-chars (numeric-chars)])
    (check-not-false (member #\0 num-chars))
    (check-not-false (member #\9 num-chars)))
  (let ([upper-chars (uppercase-chars)])
    (check-not-false (member #\A upper-chars)))
  (let ([lower-chars (lowercase-chars)])
    (check-not-false (member #\z lower-chars)))
  (let ([blank-chars (blank-chars)])
    (check-not-false (member #\space blank-chars))
    (check-not-false (member #\tab blank-chars))))

(test-case "non-word-chars: contains expected non-word characters"
  (let ([non-word (non-word-chars)])
    (check-not-false (member #\! non-word))
    (check-not-false (member #\@ non-word))
    (check-not-false (member #\# non-word))))

(test-case "hex-digit-chars: contains all hex digits"
  (let ([hex-chars (hex-digit-chars)])
    (check-not-false (member #\0 hex-chars))
    (check-not-false (member #\9 hex-chars))
    (check-not-false (member #\A hex-chars))
    (check-not-false (member #\F hex-chars))
    (check-not-false (member #\a hex-chars))
    (check-not-false (member #\f hex-chars))))

;; Run tests
(void)
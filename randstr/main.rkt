#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random
         (prefix-in tokenizer: "tokenizer.rkt")
         (prefix-in cc: "char-classes.rkt")
         (prefix-in gen: "generator.rkt")
         "tokenizer.rkt")

(provide
 (contract-out
  [randstr (string? . -> . string?)]
  [randstr* (string? exact-positive-integer? . -> . (listof string?))]
  [parse-and-generate (string? . -> . string?)]
  [tokenize-pattern (string? . -> . (listof (struct/c token any/c any/c any/c)))]
  [parse-character-class (list? . -> . (values vector? list?))]
  ;; NOTE: parse-quantifier 可能返回整数或 (list 'normal ...) / (list 'normal-range ...)
  [parse-quantifier (list? . -> . (values any/c list?))]
  [parse-group (list? . -> . (values string? list?))]
  [parse-unicode-property (list? . -> . (values string? list?))]
  [range->list (char? char? . -> . (listof char?))]))

;; Re-export range->list from tokenizer
(define range->list tokenizer:range->list)

;; Generate a random string based on a regex-like pattern
(define (randstr pattern)
  (parse-and-generate pattern))

;; Generate multiple random strings based on a regex-like pattern
(define (randstr* pattern n)
  (for/list ([i (in-range n)])
    (randstr pattern)))

;; Parse the pattern and generate a random string
(define (parse-and-generate pattern)
  (gen:generate-from-tokens (tokenize-pattern pattern)))

;; Tokenize the pattern into elements and quantifiers
(define (tokenize-pattern pattern)
  (tokenizer:tokenize-pattern pattern))

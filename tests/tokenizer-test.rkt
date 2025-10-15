#lang racket/base

(require rackunit
         "../randstr/tokenizer.rkt")

;; Test cases for tokenizer module

(test-case "tokenize-pattern: literal characters"
  (let ([tokens (tokenize-pattern "abc")])
    (check-equal? tokens '((literal #\a) (literal #\b) (literal #\c)))))

(test-case "tokenize-pattern: escape sequences"
  (let ([tokens (tokenize-pattern "\\w\\d\\s\\W\\D\\S")])
    (check-equal? tokens '((word-char) (digit-char) (whitespace-char) (non-word-char) (non-digit-char) (non-whitespace-char)))))

(test-case "tokenize-pattern: character class"
  (let ([tokens (tokenize-pattern "[abc]")])
    (let ([char-class-token (car tokens)])
      (check-equal? (car char-class-token) 'char-class)
      (check-true (list? (cadr char-class-token)))
      (let ([options (cadr char-class-token)])
        (check-not-false (member #\a options))
        (check-not-false (member #\b options))
        (check-not-false (member #\c options))))))

(test-case "tokenize-pattern: quantifiers"
  (let ([tokens (tokenize-pattern "a*b+c?d{3}")])
    (check-equal? (car tokens) '(literal #\a star))
    (check-equal? (cadr tokens) '(literal #\b plus))
    (check-equal? (caddr tokens) '(literal #\c optional))
    (check-equal? (cadddr tokens) '(literal #\d 3))))

(test-case "parse-character-class: simple range"
  (let-values ([(options remaining) (parse-character-class (string->list "a-z]"))])
    (check-not-false (member #\a options))
    (check-not-false (member #\c options))
    (check-not-false (member #\z options))
    (check-equal? remaining '())))

(test-case "parse-character-class: POSIX character classes"
  (let-values ([(options remaining) (parse-character-class (string->list "[:digit:]ab]"))])
    (check-not-false (member #\0 options))
    (check-not-false (member #\5 options))
    (check-not-false (member #\a options))
    (check-not-false (member #\b options))))

(test-case "parse-quantifier: fixed count"
  (let-values ([(count remaining) (parse-quantifier (string->list "5}xyz"))])
    (check-equal? count 5)
    (check-equal? (list->string remaining) "xyz")))

(test-case "parse-quantifier: invalid quantifier"
  (let-values ([(count remaining) (parse-quantifier (string->list "abc"))])
    (check-equal? count 1)))

(test-case "parse-group: simple group"
  (let-values ([(group remaining) (parse-group (string->list "abc)def") 1)])
    (check-equal? group "abc")
    (check-equal? (list->string remaining) "def")))

(test-case "parse-group: nested group"
  (let-values ([(group remaining) (parse-group (string->list "a(bc)d)e") 1)])
    (check-equal? group "a(bc)d")
    (check-equal? (list->string remaining) "e")))

;; Run tests
(void)
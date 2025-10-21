#lang racket/base

(require rackunit
         "../tokenizer.rkt")

;; Test cases for tokenizer module

(test-case "tokenize-pattern: literal characters"
  (let ([tokens (tokenize-pattern "abc")])
    (check-equal? tokens (list (token 'literal #\a #f) (token 'literal #\b #f) (token 'literal #\c #f)))))

(test-case "tokenize-pattern: escape sequences"
  (let ([tokens (tokenize-pattern "\\w\\d\\s\\W\\D\\S")])
    (check-equal? tokens (list (token 'word-char #f #f) (token 'digit-char #f #f) (token 'whitespace-char #f #f) (token 'non-word-char #f #f) (token 'non-digit-char #f #f) (token 'non-whitespace-char #f #f)))))

(test-case "tokenize-pattern: character class"
  (let ([tokens (tokenize-pattern "[abc]")])
    (let ([char-class-token (car tokens)])
      (check-equal? (token-type char-class-token) 'char-class)
      (check-true (vector? (token-content char-class-token)))
      (let ([options (token-content char-class-token)])
        (check-not-false (member #\a (vector->list options)))
        (check-not-false (member #\b (vector->list options)))
        (check-not-false (member #\c (vector->list options)))))))

(test-case "tokenize-pattern: quantifiers"
  (let ([tokens (tokenize-pattern "a*b+c?d{3}")])
    (check-equal? (car tokens) (token 'literal #\a 'star))
    (check-equal? (cadr tokens) (token 'literal #\b 'plus))
    (check-equal? (caddr tokens) (token 'literal #\c 'optional))
    (check-equal? (cadddr tokens) (token 'literal #\d 3))))

(test-case "parse-character-class: simple range"
  (let-values ([(options remaining) (parse-character-class (string->list "a-z]"))])
    (check-true (vector? options))
    (let ([options-list (vector->list options)])
      (check-not-false (member #\a options-list))
      (check-not-false (member #\c options-list))
      (check-not-false (member #\z options-list)))
    (check-equal? remaining '())))

(test-case "parse-character-class: POSIX character classes"
  (let-values ([(options remaining) (parse-character-class (string->list "[:digit:]ab]"))])
    (check-true (vector? options))
    (let ([options-list (vector->list options)])
      (check-not-false (member #\0 options-list))
      (check-not-false (member #\5 options-list))
      (check-not-false (member #\a options-list))
      (check-not-false (member #\b options-list)))))

(test-case "parse-quantifier: fixed count"
  (let-values ([(count remaining) (parse-quantifier (string->list "5}xyz"))])
    (check-equal? count 5)
    (check-equal? (list->string remaining) "xyz")))

(test-case "parse-quantifier: invalid quantifier"
  (let-values ([(count remaining) (parse-quantifier (string->list "abc"))])
    (check-equal? count 1)))

(test-case "parse-group: simple group"
  (let-values ([(group remaining) (parse-group (string->list "abc)def"))])
    (check-equal? group "abc")
    (check-equal? (list->string remaining) "def")))

(test-case "parse-group: nested group"
  (let-values ([(group remaining) (parse-group (string->list "a(bc)d)e"))])
    (check-equal? group "a(bc)d")
    (check-equal? (list->string remaining) "e")))

;; Run tests
(void)
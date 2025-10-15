#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random)

(provide
 (contract-out
  [random-character (-> char?)]
  [random-word-char (-> char?)]
  [random-whitespace-char (-> char?)]
  [random-non-whitespace-char (-> char?)]
  [random-non-word-char (-> char?)]
  [random-digit-char (-> char?)]
  [random-non-digit-char (-> char?)]
  [random-ref ((listof any/c) . -> . any/c)]
  [alphanumeric-chars (-> (listof char?))]
  [alphabetic-chars (-> (listof char?))]
  [numeric-chars (-> (listof char?))]
  [non-word-chars (-> (listof char?))]
  [non-digit-chars (-> (listof char?))]
  [uppercase-chars (-> (listof char?))]
  [lowercase-chars (-> (listof char?))]
  [blank-chars (-> (listof char?))]
  [punctuation-chars (-> (listof char?))]
  [control-chars (-> (listof char?))]
  [printable-chars (-> (listof char?))]
  [graphic-chars (-> (listof char?))]
  [ascii-chars (-> (listof char?))]
  [hex-digit-chars (-> (listof char?))]))

;; Generate a random character
(define (random-character)
  (let ([chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"])
    (string-ref chars (random (string-length chars)))))

;; Generate a random word character (alphanumeric + underscore)
(define (random-word-char)
  (let ([chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"])
    (string-ref chars (random (string-length chars)))))

;; Generate a random whitespace character
(define (random-whitespace-char)
  (random-ref '(#\space #\tab #\newline #\return)))

;; Generate a random non-whitespace character
(define (random-non-whitespace-char)
  (random-ref (alphanumeric-chars)))

;; Generate a random non-word character
(define (random-non-word-char)
  (random-ref (non-word-chars)))

;; Generate a random digit character
(define (random-digit-char)
  (random-ref (numeric-chars)))

;; Generate a random non-digit character
(define (random-non-digit-char)
  (random-ref (non-digit-chars)))

;; Get a random element from a list
(define (random-ref lst)
  (list-ref lst (random (length lst))))

;; Generate list of alphanumeric characters
(define (alphanumeric-chars)
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

;; Generate list of alphabetic characters
(define (alphabetic-chars)
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;; Generate list of numeric characters
(define (numeric-chars)
  (string->list "0123456789"))

;; Generate list of non-word characters (not alphanumeric or underscore)
(define (non-word-chars)
  (string->list "!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"))

;; Generate list of non-digit characters
(define (non-digit-chars)
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"))

;; Generate list of upper case characters
(define (uppercase-chars)
  (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;; Generate list of lower case characters
(define (lowercase-chars)
  (string->list "abcdefghijklmnopqrstuvwxyz"))

;; Generate list of blank characters (space and tab)
(define (blank-chars)
  (list #\space #\tab))

;; Generate list of punctuation characters
(define (punctuation-chars)
  (string->list "!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"))

;; Generate list of control characters (ASCII 0-31)
(define (control-chars)
  (for/list ([i (in-range 0 32)])
    (integer->char i)))

;; Generate list of printable characters (ASCII 32-126)
(define (printable-chars)
  (for/list ([i (in-range 32 127)])
    (integer->char i)))

;; Generate list of graphic characters (printable except space)
(define (graphic-chars)
  (for/list ([i (in-range 33 127)])
    (integer->char i)))

;; Generate list of ASCII characters (0-127)
(define (ascii-chars)
  (for/list ([i (in-range 0 128)])
    (integer->char i)))

;; Generate list of hexadecimal digits
(define (hex-digit-chars)
  (string->list "0123456789ABCDEFabcdef"))
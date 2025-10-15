#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random)

(provide
 (contract-out
  [tokenize-pattern (string? . -> . (listof (listof any/c)))]
  [parse-character-class (list? . -> . (values (listof char?) list?))]
  [parse-quantifier (list? . -> . (values exact-integer? list?))]
  [parse-group (list? exact-integer? . -> . (values string? list?))]))

;; Tokenize the pattern into elements and quantifiers
(define (tokenize-pattern pattern)
  (let loop ([chars (string->list pattern)]
             [tokens '()])
    (cond
      [(null? chars) (reverse tokens)]
      [(and (>= (length chars) 2)
            (char=? (car chars) #\\))
       ;; Handle escape sequences
       (let ([escape-char (cadr chars)])
         (case escape-char
           [(#\w) (loop (cddr chars) (cons (list 'word-char) tokens))]
           [(#\W) (loop (cddr chars) (cons (list 'non-word-char) tokens))]
           [(#\s) (loop (cddr chars) (cons (list 'whitespace-char) tokens))]
           [(#\S) (loop (cddr chars) (cons (list 'non-whitespace-char) tokens))]
           [(#\d) (loop (cddr chars) (cons (list 'digit-char) tokens))]
           [(#\D) (loop (cddr chars) (cons (list 'non-digit-char) tokens))]
           [else (loop (cddr chars) (cons (list 'literal escape-char) tokens))]))]
      [(char=? (car chars) #\[)
       (let-values ([(options remaining) (parse-character-class (cdr chars))])
         (loop remaining (cons (list 'char-class options) tokens)))]
      [(char=? (car chars) #\()
       (let-values ([(group remaining) (parse-group (cdr chars) 1)])
         (loop remaining (cons (list 'group group) tokens)))]
      [(char=? (car chars) #\{)
       (if (null? tokens)
           (loop (cdr chars) tokens)
           (let-values ([(count remaining) (parse-quantifier (cdr chars))])
             ;; Update the last token with its quantifier
             (let ([last-token (car tokens)]
                   [rest-tokens (cdr tokens)])
               (loop remaining (cons (append last-token (list count)) rest-tokens)))))]
      [(char=? (car chars) #\*)
       (if (null? tokens)
           (loop (cdr chars) tokens)
           (let ([last-token (car tokens)]
                 [rest-tokens (cdr tokens)])
             (loop (cdr chars) (cons (append last-token (list 'star)) rest-tokens))))]
      [(char=? (car chars) #\+)
       (if (null? tokens)
           (loop (cdr chars) tokens)
           (let ([last-token (car tokens)]
                 [rest-tokens (cdr tokens)])
             (loop (cdr chars) (cons (append last-token (list 'plus)) rest-tokens))))]
      [(char=? (car chars) #\?)
       (if (null? tokens)
           (loop (cdr chars) tokens)
           (let ([last-token (car tokens)]
                 [rest-tokens (cdr tokens)])
             (loop (cdr chars) (cons (append last-token (list 'optional)) rest-tokens))))]
      [(char=? (car chars) #\.)
       (loop (cdr chars) (cons (list 'any) tokens))]
      [else
       (loop (cdr chars) (cons (list 'literal (car chars)) tokens))])))

;; Parse a character class like [abc] or [a-z]
(define (parse-character-class chars)
  (let loop ([remaining chars]
             [options '()]
             [in-range? #f]
             [range-start #f])
    (cond
      [(null? remaining)
       (values (reverse (remove-duplicates-preserving-order (reverse options))) remaining)]
      ;; Handle nested POSIX character classes like [[:alpha:][:digit:]]
      [(and (>= (length remaining) 3)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:))
       ;; Parse POSIX character class
       (let-values ([(posix-chars new-remaining) (parse-posix-character-class remaining)])
         (loop new-remaining
               (append posix-chars options)
               #f #f))]
      [(char=? (car remaining) #\])
       (if (null? options)
           (values '(#\]) (cdr remaining))
           (values (reverse (remove-duplicates-preserving-order (reverse options))) (cdr remaining)))]
      [(and in-range? range-start (char<=? range-start (car remaining)))
       ;; Add range of characters
       (loop (cdr remaining)
             (append (range->list range-start (car remaining)) options)
             #f #f)]
      [(char=? (car remaining) #\-)
       (if (null? options)
           (loop (cdr remaining) (cons #\- options) #f #f)
           (loop (cdr remaining) options #t (car options)))]
      [else
       (loop (cdr remaining) (cons (car remaining) options) #f #f)])))

;; Parse a POSIX character class like [:alpha:] or nested ones
(define (parse-posix-character-class chars)
  (cond
    ;; Check if it's a valid POSIX character class start
    [(and (>= (length chars) 3)
          (char=? (car chars) #\[)
          (char=? (cadr chars) #\:))
     ;; Find the matching closing :]
     (let loop ([remaining (cddr chars)]
                [class-name-chars '()])
       (cond
         [(null? remaining)
          ;; If we can't find the end, treat as literal
          (values '(#\[ #\:) (cdr chars))]
         [(and (>= (length remaining) 2)
               (char=? (car remaining) #\:)
               (char=? (cadr remaining) #\]))
          ;; Found the end of POSIX character class
          (let ([class-name (list->string (reverse class-name-chars))])
            (values (posix-class->chars class-name) (cddr remaining)))]
         [else
          (loop (cdr remaining) (cons (car remaining) class-name-chars))]))]
    [else
     ;; Not a valid POSIX character class start
     (values '() chars)]))

;; Convert POSIX character class name to list of characters
(define (posix-class->chars class-name)
  (cond
    [(string=? class-name "alpha")
     (alphabetic-chars)]
    [(string=? class-name "digit")
     (numeric-chars)]
    [(string=? class-name "alphanum")
     (alphanumeric-chars)]
    [(string=? class-name "alnum")
     (alphanumeric-chars)]
    [(string=? class-name "word")
     (append (alphanumeric-chars) (list #\_))]
    [(string=? class-name "blank")
     (blank-chars)]
    [(string=? class-name "space")
     (append (blank-chars) (list #\newline #\return))]
    [(string=? class-name "upper")
     (uppercase-chars)]
    [(string=? class-name "lower")
     (lowercase-chars)]
    [(string=? class-name "ascii")
     (ascii-chars)]
    [(string=? class-name "cntrl")
     (control-chars)]
    [(string=? class-name "graph")
     (graphic-chars)]
    [(string=? class-name "print")
     (printable-chars)]
    [(string=? class-name "punct")
     (punctuation-chars)]
    [(string=? class-name "xdigit")
     (hex-digit-chars)]
    [else
     ;; Unknown POSIX class, return empty list
     '()]))

;; Parse a quantifier like {5} or {2,5}
(define (parse-quantifier chars)
  (let loop ([remaining chars]
             [digits '()])
    (cond
      [(null? remaining)
       (values (string->number (list->string (reverse digits))) remaining)]
      [(char=? (car remaining) #\})
       (let ([count (string->number (list->string (reverse digits)))])
         (values (if count count 1) (cdr remaining)))]
      [(char-numeric? (car remaining))
       (loop (cdr remaining) (cons (car remaining) digits))]
      [else
       (values 1 remaining)])))

;; Parse a group like (abc)
(define (parse-group chars nesting)
  (let loop ([remaining chars]
             [group-chars '()]
             [nesting nesting])
    (cond
      [(null? remaining)
       (values (list->string (reverse group-chars)) remaining)]
      [(and (char=? (car remaining) #\)) (= nesting 1))
       (values (list->string (reverse group-chars)) (cdr remaining))]
      [(char=? (car remaining) #\()
       (loop (cdr remaining) (cons (car remaining) group-chars) (+ nesting 1))]
      [(char=? (car remaining) #\))
       (loop (cdr remaining) (cons (car remaining) group-chars) (- nesting 1))]
      [else
       (loop (cdr remaining) (cons (car remaining) group-chars) nesting)])))

;; Convert a character range to a list of characters
(define (range->list start end)
  (for/list ([i (in-range (char->integer start) (+ 1 (char->integer end)))])
    (integer->char i)))

;; Remove duplicates while preserving order
(define (remove-duplicates-preserving-order lst)
  (let loop ([lst lst]
             [seen '()]
             [result '()])
    (cond
      [(null? lst) (reverse result)]
      [(member (car lst) seen)
       (loop (cdr lst) seen result)]
      [else
       (loop (cdr lst) (cons (car lst) seen) (cons (car lst) result))])))

;; Generate list of alphanumeric characters
(define (alphanumeric-chars)
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

;; Generate list of alphabetic characters
(define (alphabetic-chars)
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;; Generate list of numeric characters
(define (numeric-chars)
  (string->list "0123456789"))

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

;; Generate list of upper case characters
(define (uppercase-chars)
  (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;; Generate list of lower case characters
(define (lowercase-chars)
  (string->list "abcdefghijklmnopqrstuvwxyz"))
#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random)

(provide
 (contract-out
  [randstr (string? . -> . string?)]
  [randstr* (string? exact-positive-integer? . -> . (listof string?))]
  [parse-and-generate (string? . -> . string?)]
  [tokenize-pattern (string? . -> . (listof (listof any/c)))]
  [generate-from-tokens ((listof (listof any/c)) . -> . string?)]
  [parse-character-class (list? . -> . (values (listof char?) list?))]
  [range->list (char? char? . -> . (listof char?))]))

;; Generate a random string based on a regex-like pattern
(define (randstr pattern)
  (parse-and-generate pattern))

;; Generate multiple random strings based on a regex-like pattern
(define (randstr* pattern n)
  (for/list ([i (in-range n)])
    (randstr pattern)))

;; Parse the pattern and generate a random string
(define (parse-and-generate pattern)
  (generate-from-tokens (tokenize-pattern pattern)))

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
       (let-values ([(group remaining) (parse-group (cdr chars))])
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

;; Generate string from tokens
(define (generate-from-tokens tokens)
  (let loop ([tokens tokens]
             [result '()])
    (cond
      [(null? tokens) (list->string (reverse result))]
      [else
       (let ([token (car tokens)])
         (case (car token)
           [(literal)
            (let ([char (cadr token)])
              (cond
                [(and (>= (length token) 3) (number? (caddr token))) ; {n}
                 (loop (cdr tokens) (append (make-list (caddr token) char) result))]
                [(and (>= (length token) 3) (eq? (caddr token) 'star)) ; *
                 (let ([count (random 5)])
                   (loop (cdr tokens) (append (make-list count char) result)))]
                [(and (>= (length token) 3) (eq? (caddr token) 'plus)) ; +
                 (let ([count (+ 1 (random 5))])
                   (loop (cdr tokens) (append (make-list count char) result)))]
                [(and (>= (length token) 3) (eq? (caddr token) 'optional)) ; ?
                 (if (zero? (random 2))
                     (loop (cdr tokens) result)
                     (loop (cdr tokens) (cons char result)))]
                [else
                 (loop (cdr tokens) (cons char result))]))]
           [(char-class)
            (let ([options (cadr token)])
              (let ([char (random-ref options)])
                (cond
                  [(and (>= (length token) 3) (number? (caddr token))) ; {n}
                   ;; For character classes with {n}, we generate n different random chars
                   (let ([chars (for/list ([i (in-range (caddr token))])
                                  (random-ref options))])
                     (loop (cdr tokens) (append (reverse chars) result)))]
                  [(and (>= (length token) 3) (eq? (caddr token) 'star)) ; *
                   (let ([count (random 5)])
                     (let ([chars (for/list ([i (in-range count)])
                                    (random-ref options))])
                       (loop (cdr tokens) (append (reverse chars) result))))]
                  [(and (>= (length token) 3) (eq? (caddr token) 'plus)) ; +
                   (let ([count (+ 1 (random 5))])
                     (let ([chars (for/list ([i (in-range count)])
                                    (random-ref options))])
                       (loop (cdr tokens) (append (reverse chars) result))))]
                  [(and (>= (length token) 3) (eq? (caddr token) 'optional)) ; ?
                   (if (zero? (random 2))
                       (loop (cdr tokens) result)
                       (loop (cdr tokens) (cons char result)))]
                  [else
                   (loop (cdr tokens) (cons char result))])))]
           [(word-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For word-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (random-word-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-word-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-word-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (random-word-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (random-word-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(whitespace-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For whitespace-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (random-whitespace-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-whitespace-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-whitespace-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (random-whitespace-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (random-whitespace-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(non-whitespace-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For non-whitespace-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (random-non-whitespace-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-non-whitespace-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-non-whitespace-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (random-non-whitespace-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (random-non-whitespace-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(non-word-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For non-word-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (random-non-word-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-non-word-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-non-word-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (random-non-word-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (random-non-word-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(digit-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For digit-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (random-digit-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-digit-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-digit-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (random-digit-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (random-digit-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(non-digit-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For non-digit-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (random-non-digit-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-non-digit-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (random-non-digit-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (random-non-digit-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (random-non-digit-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(any)
            (let ([char (random-character)])
              (loop (cdr tokens) (cons char result)))]
           [(group)
            ;; For simplicity, we'll just generate a string from the group pattern
            ;; A full implementation would parse the group content
            (loop (cdr tokens) (cons #\g result))] ; placeholder
           [else
            (loop (cdr tokens) result)]
           ))])))

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
(define (parse-group chars)
  (let loop ([remaining chars]
             [group-chars '()]
             [nesting 1])
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
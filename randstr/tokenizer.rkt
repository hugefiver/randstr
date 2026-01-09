#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random
         "utils.rkt"
         racket/match
         (for-syntax racket/base))

(provide
 (contract-out
  [tokenize-pattern (string? . -> . (listof (struct/c token any/c any/c any/c)))]
  [parse-character-class (list? . -> . (values vector? list?))]
  [parse-quantifier (list? . -> . (values brace-quantifier/c list?))]
  [parse-group (list? . -> . (values string? list?))]
  [parse-unicode-property (list? . -> . (values string? list?))]
  [range->list (char? char? . -> . (listof char?))])
 (struct-out token))

;; Quantifier result for {...} parsing.
;; Supports:
;;   - exact-nonnegative-integer?             ; {n}
;;   - (list 'normal n order)                 ; {n+}, {n++}, ...
;;   - (list 'normal-range n1 n2 order)       ; {n1+n2}, {+n2}, {n1++n2}, ...
(define brace-quantifier/c
  (or/c
   exact-nonnegative-integer?
   (list/c 'normal exact-nonnegative-integer? exact-positive-integer?)
   (list/c 'normal-range exact-nonnegative-integer? exact-nonnegative-integer? exact-positive-integer?)))

;; Define token structure for better organization
;; quantifier can be:
;;   - #f: no quantifier
;;   - 'star, 'plus, 'optional: *, +, ?
;;   - exact integer: {n}
;;   - (list 'normal n order): {n+}, {n++}, etc. for normal distribution
(struct token (type content quantifier) #:transparent)

;; Tokenize the pattern into elements and quantifiers
(define (tokenize-pattern pattern)
  (let loop ([chars (string->list pattern)]
             [tokens '()])
    (match chars
      ['() (reverse tokens)]

      [(cons #\^ rest)
       ;; Start anchor - skip
       (loop rest tokens)]

      [(cons #\$ rest)
       ;; End anchor - skip
       (loop rest tokens)]

      [(cons #\[ rest)
       (let-values ([(options remaining) (parse-character-class rest)])
         (loop remaining (cons (token 'char-class options #f) tokens)))]

      [(cons #\( rest)
       ;; Named group (?<name>...)
       (match rest
         [(list* #\? #\< more)
          (let-values ([(name group remaining) (parse-named-group more)])
            (loop remaining (cons (token 'named-group (cons name group) #f) tokens)))]
         [_
          (let-values ([(group remaining) (parse-group-internal rest 1)])
            (loop remaining (cons (token 'group group #f) tokens)))])]

      [(cons #\{ rest)
       (if (null? tokens)
           (loop rest tokens)
           (let-values ([(count remaining) (parse-quantifier rest)])
             (let ([last-token (car tokens)]
                   [rest-tokens (cdr tokens)])
               (loop remaining (cons (struct-copy token last-token [quantifier count]) rest-tokens)))))]

      [(cons #\* rest)
       (if (null? tokens)
           (loop rest tokens)
           (let ([last-token (car tokens)]
                 [rest-tokens (cdr tokens)])
             (loop rest (cons (struct-copy token last-token [quantifier 'star]) rest-tokens))))]

      [(cons #\+ rest)
       (if (null? tokens)
           (loop rest tokens)
           (let ([last-token (car tokens)]
                 [rest-tokens (cdr tokens)])
             (loop rest (cons (struct-copy token last-token [quantifier 'plus]) rest-tokens))))]

      [(cons #\? rest)
       (if (null? tokens)
           (loop rest tokens)
           (let ([last-token (car tokens)]
                 [rest-tokens (cdr tokens)])
             (loop rest (cons (struct-copy token last-token [quantifier 'optional]) rest-tokens))))]

      [(cons #\. rest)
       (loop rest (cons (token 'any #f #f) tokens))]

      [(list* #\\ escape-char rest)
       ;; Escape sequences
       (case escape-char
         [(#\w) (loop rest (cons (token 'word-char #f #f) tokens))]
         [(#\W) (loop rest (cons (token 'non-word-char #f #f) tokens))]
         [(#\s) (loop rest (cons (token 'whitespace-char #f #f) tokens))]
         [(#\S) (loop rest (cons (token 'non-whitespace-char #f #f) tokens))]
         [(#\d) (loop rest (cons (token 'digit-char #f #f) tokens))]
         [(#\D) (loop rest (cons (token 'non-digit-char #f #f) tokens))]
         [(#\p)
          (match rest
            [(cons #\{ more)
             (let-values ([(property remaining) (parse-unicode-property more)])
               (loop remaining (cons (token 'unicode-property property #f) tokens)))]
            [_ (loop rest (cons (token 'literal escape-char #f) tokens))])]
         [(#\k)
          (match rest
            [(cons #\< more)
             (let-values ([(name remaining) (parse-backreference-name more)])
               (loop remaining (cons (token 'backreference name #f) tokens)))]
            [_ (loop rest (cons (token 'literal escape-char #f) tokens))])]
         [else (loop rest (cons (token 'literal escape-char #f) tokens))])]

      [(cons c rest)
       (loop rest (cons (token 'literal c #f) tokens))])))

;; Parse a character class like [abc] or [a-z]
(define (parse-character-class chars)
  ;; Support negated classes like [^abc].
  ;; For now, we compute the complement within ASCII printable characters.
  (define negated?
    (and (pair? chars) (char=? (car chars) #\^)))
  (define start-remaining (if negated? (cdr chars) chars))

  ;; Expand escape sequences inside character classes.
  ;; Returns (values expanded-chars remaining-chars) where expanded-chars is a list of chars.
  (define (class-escape->chars rem)
    (cond
      [(or (null? rem) (null? (cdr rem)))
       (values (list #\\) (if (null? rem) rem (cdr rem)))]
      [else
       (define esc (cadr rem))
       (case esc
         [(#\d) (values (numeric-chars) (cddr rem))]
         [(#\D) (values (non-digit-chars) (cddr rem))]
         [(#\w) (values (append (alphanumeric-chars) (list #\_)) (cddr rem))]
         [(#\W) (values (non-word-chars) (cddr rem))]
         [(#\s) (values (list #\space #\tab #\newline #\return) (cddr rem))]
         [(#\S)
          (define ws (list #\space #\tab #\newline #\return))
          (values (filter (lambda (c) (not (member c ws))) (printable-chars)) (cddr rem))]
         ;; Common literal escapes inside []
         [(#\] #\- #\\) (values (list esc) (cddr rem))]
         [else (values (list esc) (cddr rem))])]))

  (let loop ([remaining start-remaining]
             [options '()]
             [in-range? #f]
             [range-start #f])
    (cond
      [(null? remaining)
       (let* ([unique-options (remove-duplicates-preserving-order (reverse options))]
              [final-options (if negated?
                                 (filter (lambda (c) (not (member c unique-options)))
                                         (printable-chars))
                                 unique-options)])
         (values (list->vector final-options) remaining))]
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
           (values (vector #\]) (cdr remaining))
            (let* ([unique-options (remove-duplicates-preserving-order (reverse options))]
              [final-options (if negated?
                  (filter (lambda (c) (not (member c unique-options)))
                     (printable-chars))
                  unique-options)])
              (values (list->vector final-options) (cdr remaining))))]

            [(char=? (car remaining) #\\)
        (let-values ([(expanded new-remaining) (class-escape->chars remaining)])
          ;; Escapes expand to literal chars/options; they do not participate in ranges.
               (loop new-remaining (append (reverse expanded) options) #f #f))]
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

;; Parse a quantifier like {5} or {5+} or {5++} for normal distribution
;; {n} returns n
;; {n+} returns (list 'normal n 2) for 2nd order normal distribution
;; {n++} returns (list 'normal n 3) for 3rd order, etc.
;; {n1+n2} returns (list 'normal-range n1 n2 2) for range normal distribution
;; {n1++n2} returns (list 'normal-range n1 n2 3) for 3rd order range normal
;; {+n} or {++n} is shorthand for {0+n} or {0++n}
(define (parse-quantifier chars)
  (let loop ([remaining chars]
             [digits '()])
    (cond
      [(null? remaining)
       (values (string->number (list->string (reverse digits))) remaining)]
      [(char=? (car remaining) #\})
       (let ([count (string->number (list->string (reverse digits)))])
         (values (if count count 1) (cdr remaining)))]
      [(char=? (car remaining) #\+)
       ;; Count the number of + signs for normal distribution order
       (let count-plus ([rest (cdr remaining)]
                        [plus-count 1])
         (cond
           [(null? rest)
            (let ([n (string->number (list->string (reverse digits)))])
              (values (list 'normal (if n n 0) (+ plus-count 1)) rest))]
           [(char=? (car rest) #\+)
            (count-plus (cdr rest) (+ plus-count 1))]
           [(char=? (car rest) #\})
            (let ([n (string->number (list->string (reverse digits)))])
              (values (list 'normal (if n n 0) (+ plus-count 1)) (cdr rest)))]
           [(char-numeric? (car rest))
            ;; This is a range: {n1+...n2} or {+...n2}
            ;; Parse the second number
            (let parse-second ([rest2 rest]
                               [digits2 '()])
              (cond
                [(null? rest2)
                 (let ([n1 (string->number (list->string (reverse digits)))]
                       [n2 (string->number (list->string (reverse digits2)))])
                   (values (list 'normal-range (if n1 n1 0) (if n2 n2 1) (+ plus-count 1)) rest2))]
                [(char=? (car rest2) #\})
                 (let ([n1 (string->number (list->string (reverse digits)))]
                       [n2 (string->number (list->string (reverse digits2)))])
                   (values (list 'normal-range (if n1 n1 0) (if n2 n2 1) (+ plus-count 1)) (cdr rest2)))]
                [(char-numeric? (car rest2))
                 (parse-second (cdr rest2) (cons (car rest2) digits2))]
                [else
                 (let ([n1 (string->number (list->string (reverse digits)))]
                       [n2 (string->number (list->string (reverse digits2)))])
                   (values (list 'normal-range (if n1 n1 0) (if n2 n2 1) (+ plus-count 1)) rest2))]))]
           [else
            (let ([n (string->number (list->string (reverse digits)))])
              (values (list 'normal (if n n 0) (+ plus-count 1)) rest))]))]
      [(char-numeric? (car remaining))
       (loop (cdr remaining) (cons (car remaining) digits))]
      [else
       (values 1 remaining)])))

;; Parse a group like (abc)
(define (parse-group chars)
  (parse-group-internal chars 1))

;; Internal function to parse a group with nesting level
(define (parse-group-internal chars nesting)
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
      [(and (char=? (car remaining) #\|) (= nesting 1))
       ;; For now, we'll just collect the group content including the | operator
       ;; The generator will handle the branching logic
       (loop (cdr remaining) (cons (car remaining) group-chars) nesting)]
      [(char=? (car remaining) #\))
       (loop (cdr remaining) (cons (car remaining) group-chars) (- nesting 1))]
      [else
       (loop (cdr remaining) (cons (car remaining) group-chars) nesting)])))

;; Parse a named group like (?<name>...) - starts after the "<"
;; Returns (values name group-content remaining-chars)
(define (parse-named-group chars)
  (let name-loop ([remaining chars]
                  [name-chars '()])
    (cond
      [(null? remaining)
       ;; No closing > found, return empty
       (values "" "" remaining)]
      [(char=? (car remaining) #\>)
       ;; Found end of name, now parse the group content
       (let ([name (list->string (reverse name-chars))])
         (let-values ([(group rest) (parse-group-internal (cdr remaining) 1)])
           (values name group rest)))]
      [else
       (name-loop (cdr remaining) (cons (car remaining) name-chars))])))

;; Parse a backreference name like \k<name> - starts after the "<"
;; Returns (values name remaining-chars)
(define (parse-backreference-name chars)
  (let loop ([remaining chars]
             [name-chars '()])
    (cond
      [(null? remaining)
       (values (list->string (reverse name-chars)) remaining)]
      [(char=? (car remaining) #\>)
       (values (list->string (reverse name-chars)) (cdr remaining))]
      [else
       (loop (cdr remaining) (cons (car remaining) name-chars))])))

;; Parse a Unicode property like {L} or {Letter} or {Script=Han} or {Block=Basic_Latin}
(define (parse-unicode-property chars)
  (let loop ([remaining chars]
             [property-chars '()])
    (cond
      [(null? remaining)
       ;; If we can't find the end, treat as literal
       (values "" remaining)]
      [(char=? (car remaining) #\})
       ;; Found the end of Unicode property
       (let ([property-name (list->string (reverse property-chars))])
         (values property-name (cdr remaining)))]
      [else
       (loop (cdr remaining) (cons (car remaining) property-chars))])))

;; Convert a character range to a list of characters
(define (range->list start end)
  (for/list ([i (in-range (char->integer start) (+ 1 (char->integer end)))])
    (integer->char i)))


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

;; Generate list of non-word characters (not alphanumeric or underscore)
(define (non-word-chars)
  (string->list "!@#$%^&*()-+={}[]|\\:;\"'<>?,./`~"))

;; Generate list of non-digit characters
(define (non-digit-chars)
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"))

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
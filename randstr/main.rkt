#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random)

(provide
 (contract-out
  [randstr (string? . -> . string?)]
  [randstr* (string? exact-positive-integer? . -> . (listof string?))]))

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
       (values (reverse options) remaining)]
      [(and (>= (length remaining) 12)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\a)
            (char=? (cadddr remaining) #\l)
            (char=? (list-ref remaining 4) #\p)
            (char=? (list-ref remaining 5) #\h)
            (char=? (list-ref remaining 6) #\a)
            (char=? (list-ref remaining 7) #\n)
            (char=? (list-ref remaining 8) #\u)
            (char=? (list-ref remaining 9) #\m)
            (char=? (list-ref remaining 10) #\:)
            (char=? (list-ref remaining 11) #\]))
       ;; Handle [:alphanum:]
       (loop (list-tail remaining 12)
             (append (alphanumeric-chars) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\a)
            (char=? (cadddr remaining) #\l)
            (char=? (list-ref remaining 4) #\p)
            (char=? (list-ref remaining 5) #\h)
            (char=? (list-ref remaining 6) #\:)
            (char=? (list-ref remaining 7) #\])
            (not in-range?))
       ;; Handle [:alpha:]
       (loop (list-tail remaining 8)
             (append (alphabetic-chars) options)
             #f #f)]
      [(and (>= (length remaining) 10)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\d)
            (char=? (cadddr remaining) #\i)
            (char=? (list-ref remaining 4) #\g)
            (char=? (list-ref remaining 5) #\i)
            (char=? (list-ref remaining 6) #\t)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\:)
            (char=? (list-ref remaining 9) #\])
            (not in-range?))
       ;; Handle [:digit:]
       (loop (list-tail remaining 10)
             (append (numeric-chars) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\a)
            (char=? (cadddr remaining) #\l)
            (char=? (list-ref remaining 4) #\n)
            (char=? (list-ref remaining 5) #\u)
            (char=? (list-ref remaining 6) #\m)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:alnum:]
       (loop (list-tail remaining 9)
             (append (alphanumeric-chars) options)
             #f #f)]
      [(and (>= (length remaining) 8)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\w)
            (char=? (cadddr remaining) #\o)
            (char=? (list-ref remaining 4) #\r)
            (char=? (list-ref remaining 5) #\d)
            (char=? (list-ref remaining 6) #\:)
            (char=? (list-ref remaining 7) #\])
            (not in-range?))
       ;; Handle [:word:]
       (loop (list-tail remaining 8)
             (append (alphanumeric-chars) (list #\_) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\b)
            (char=? (cadddr remaining) #\l)
            (char=? (list-ref remaining 4) #\a)
            (char=? (list-ref remaining 5) #\n)
            (char=? (list-ref remaining 6) #\k)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:blank:]
       (loop (list-tail remaining 9)
             (append (blank-chars) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\s)
            (char=? (cadddr remaining) #\p)
            (char=? (list-ref remaining 4) #\a)
            (char=? (list-ref remaining 5) #\c)
            (char=? (list-ref remaining 6) #\e)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:space:]
       (loop (list-tail remaining 9)
             (append (blank-chars) (list #\newline #\return) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\u)
            (char=? (cadddr remaining) #\p)
            (char=? (list-ref remaining 4) #\p)
            (char=? (list-ref remaining 5) #\e)
            (char=? (list-ref remaining 6) #\r)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:upper:]
       (loop (list-tail remaining 9)
             (append (uppercase-chars) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\l)
            (char=? (cadddr remaining) #\o)
            (char=? (list-ref remaining 4) #\w)
            (char=? (list-ref remaining 5) #\e)
            (char=? (list-ref remaining 6) #\r)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:lower:]
       (loop (list-tail remaining 9)
             (append (lowercase-chars) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\a)
            (char=? (cadddr remaining) #\s)
            (char=? (list-ref remaining 4) #\c)
            (char=? (list-ref remaining 5) #\i)
            (char=? (list-ref remaining 6) #\i)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:ascii:]
       (loop (list-tail remaining 9)
             (append (ascii-chars) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\c)
            (char=? (cadddr remaining) #\n)
            (char=? (list-ref remaining 4) #\t)
            (char=? (list-ref remaining 5) #\r)
            (char=? (list-ref remaining 6) #\l)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:cntrl:]
       (loop (list-tail remaining 9)
             (append (control-chars) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\g)
            (char=? (cadddr remaining) #\r)
            (char=? (list-ref remaining 4) #\a)
            (char=? (list-ref remaining 5) #\p)
            (char=? (list-ref remaining 6) #\h)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:graph:]
       (loop (list-tail remaining 9)
             (append (graphic-chars) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\p)
            (char=? (cadddr remaining) #\r)
            (char=? (list-ref remaining 4) #\i)
            (char=? (list-ref remaining 5) #\n)
            (char=? (list-ref remaining 6) #\t)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:print:]
       (loop (list-tail remaining 9)
             (append (printable-chars) options)
             #f #f)]
      [(and (>= (length remaining) 9)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\p)
            (char=? (cadddr remaining) #\u)
            (char=? (list-ref remaining 4) #\n)
            (char=? (list-ref remaining 5) #\c)
            (char=? (list-ref remaining 6) #\t)
            (char=? (list-ref remaining 7) #\:)
            (char=? (list-ref remaining 8) #\])
            (not in-range?))
       ;; Handle [:punct:]
       (loop (list-tail remaining 9)
             (append (punctuation-chars) options)
             #f #f)]
      [(and (>= (length remaining) 10)
            (char=? (car remaining) #\[)
            (char=? (cadr remaining) #\:)
            (char=? (caddr remaining) #\x)
            (char=? (cadddr remaining) #\d)
            (char=? (list-ref remaining 4) #\i)
            (char=? (list-ref remaining 5) #\g)
            (char=? (list-ref remaining 6) #\i)
            (char=? (list-ref remaining 7) #\t)
            (char=? (list-ref remaining 8) #\:)
            (char=? (list-ref remaining 9) #\])
            (not in-range?))
       ;; Handle [:xdigit:]
       (loop (list-tail remaining 10)
             (append (hex-digit-chars) options)
             #f #f)]
      [(char=? (car remaining) #\])
       (if (null? options)
           (values '(#\]) (cdr remaining))
           (values (reverse options) (cdr remaining)))]
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
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
           [(any)
            (let ([char (random-character)])
              (loop (cdr tokens) (cons char result)))]
           [(group)
            ;; For simplicity, we'll just generate a string from the group pattern
            ;; A full implementation would parse the group content
            (loop (cdr tokens) (cons #\g result))] ; placeholder
           [else
            (loop (cdr tokens) result)]))])))

;; Parse a character class like [abc] or [a-z]
(define (parse-character-class chars)
  (let loop ([remaining chars]
             [options '()]
             [in-range? #f]
             [range-start #f])
    (cond
      [(null? remaining) 
       (values (reverse options) remaining)]
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

;; Get a random element from a list
(define (random-ref lst)
  (list-ref lst (random (length lst))))
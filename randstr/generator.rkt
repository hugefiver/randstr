#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random
         "tokenizer.rkt"
         (prefix-in cc: "char-classes.rkt")
         "utils.rkt")

(provide
 (contract-out
  [generate-from-tokens ((listof (struct/c token any/c any/c any/c)) . -> . string?)]))

;; Apply quantifier to a character or list of characters
(define (apply-quantifier char-or-func quantifier)
  (cond
    [(and quantifier (number? quantifier)) ; {n}
     (make-list quantifier char-or-func)]
    [(and quantifier (eq? quantifier 'star)) ; *
     (let ([count (random 5)])
       (make-list count char-or-func))]
    [(and quantifier (eq? quantifier 'plus)) ; +
     (let ([count (+ 1 (random 5))])
       (make-list count char-or-func))]
    [(and quantifier (eq? quantifier 'optional)) ; ?
     (if (zero? (random 2))
         '()  ; empty list means don't add anything
         (list char-or-func))]
    [else
     (list char-or-func)]))  ; no quantifier, just return as single element list

;; Generate string from tokens
(define (generate-from-tokens tokens)
  (let loop ([tokens tokens]
             [result '()])
    (cond
      [(null? tokens) (list->string (reverse result))]
      [else
       (let ([token (car tokens)])
         (case (token-type token)
           [(literal)
            (let ([char (token-content token)])
              (let ([chars (apply-quantifier char (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(char-class)
            (let ([options (token-content token)])
              (let ([char (cc:vector-random-ref options)])
                (let ([chars (apply-quantifier char (token-quantifier token))])
                  (loop (cdr tokens) (append (reverse chars) result)))))]
           [(word-char)
            (let ([char (cc:random-word-char)])
              (let ([chars (apply-quantifier char (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(whitespace-char)
            (let ([char (cc:random-whitespace-char)])
              (let ([chars (apply-quantifier char (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(non-whitespace-char)
            (let ([char (cc:random-non-whitespace-char)])
              (let ([chars (apply-quantifier char (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(non-word-char)
            (let ([char (cc:random-non-word-char)])
              (let ([chars (apply-quantifier char (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(digit-char)
            (let ([char (cc:random-digit-char)])
              (let ([chars (apply-quantifier char (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(non-digit-char)
            (let ([char (cc:random-non-digit-char)])
              (let ([chars (apply-quantifier char (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(any)
            (let ([char (cc:random-character)])
              (let ([chars (apply-quantifier char (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(group)
            ;; For simplicity, we'll just generate a string from the group pattern
            ;; A full implementation would parse the group content
            (loop (cdr tokens) (cons #\g result))] ; placeholder
           [else
            (loop (cdr tokens) result)]
           ))])))
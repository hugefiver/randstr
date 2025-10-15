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
  [generate-from-tokens ((listof (listof any/c)) . -> . string?)]))

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
              (let ([char (cc:random-ref options)])
                (cond
                  [(and (>= (length token) 3) (number? (caddr token))) ; {n}
                   ;; For character classes with {n}, we generate n different random chars
                   (let ([chars (for/list ([i (in-range (caddr token))])
                                  (cc:random-ref options))])
                     (loop (cdr tokens) (append (reverse chars) result)))]
                  [(and (>= (length token) 3) (eq? (caddr token) 'star)) ; *
                   (let ([count (random 5)])
                     (let ([chars (for/list ([i (in-range count)])
                                    (cc:random-ref options))])
                       (loop (cdr tokens) (append (reverse chars) result))))]
                  [(and (>= (length token) 3) (eq? (caddr token) 'plus)) ; +
                   (let ([count (+ 1 (random 5))])
                     (let ([chars (for/list ([i (in-range count)])
                                    (cc:random-ref options))])
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
                              (cc:random-word-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-word-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-word-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (cc:random-word-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (cc:random-word-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(whitespace-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For whitespace-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (cc:random-whitespace-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-whitespace-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-whitespace-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (cc:random-whitespace-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (cc:random-whitespace-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(non-whitespace-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For non-whitespace-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (cc:random-non-whitespace-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-non-whitespace-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-non-whitespace-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (cc:random-non-whitespace-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (cc:random-non-whitespace-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(non-word-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For non-word-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (cc:random-non-word-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-non-word-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-non-word-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (cc:random-non-word-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (cc:random-non-word-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(digit-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For digit-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (cc:random-digit-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-digit-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-digit-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (cc:random-digit-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (cc:random-digit-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(non-digit-char)
            (cond
              [(and (>= (length token) 2) (number? (cadr token))) ; {n}
               ;; For non-digit-char with {n}, we generate n different random chars
               (let ([chars (for/list ([i (in-range (cadr token))])
                              (cc:random-non-digit-char))])
                 (loop (cdr tokens) (append (reverse chars) result)))]
              [(and (>= (length token) 2) (eq? (cadr token) 'star)) ; *
               (let ([count (random 5)])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-non-digit-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'plus)) ; +
               (let ([count (+ 1 (random 5))])
                 (let ([chars (for/list ([i (in-range count)])
                                (cc:random-non-digit-char))])
                   (loop (cdr tokens) (append (reverse chars) result))))]
              [(and (>= (length token) 2) (eq? (cadr token) 'optional)) ; ?
               (if (zero? (random 2))
                   (loop (cdr tokens) result)
                   (let ([char (cc:random-non-digit-char)])
                     (loop (cdr tokens) (cons char result))))]
              [else
               (let ([char (cc:random-non-digit-char)])
                 (loop (cdr tokens) (cons char result)))]
              )]
           [(any)
            (let ([char (cc:random-character)])
              (loop (cdr tokens) (cons char result)))]
           [(group)
            ;; For simplicity, we'll just generate a string from the group pattern
            ;; A full implementation would parse the group content
            (loop (cdr tokens) (cons #\g result))] ; placeholder
           [else
            (loop (cdr tokens) result)]
           ))])))
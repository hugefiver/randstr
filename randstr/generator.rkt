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
     (if (procedure? char-or-func)
         ;; If char-or-func is a function, call it n times to get n different characters
         (for/list ([i (in-range quantifier)])
           (char-or-func))
         ;; If char-or-func is a character, repeat it n times
         (make-list quantifier char-or-func))]
    [(and quantifier (eq? quantifier 'star)) ; *
     (let ([count (random 5)])
       (if (procedure? char-or-func)
           ;; If char-or-func is a function, call it count times to get count different characters
           (for/list ([i (in-range count)])
             (char-or-func))
           ;; If char-or-func is a character, repeat it count times
           (make-list count char-or-func)))]
    [(and quantifier (eq? quantifier 'plus)) ; +
     (let ([count (+ 1 (random 5))])
       (if (procedure? char-or-func)
           ;; If char-or-func is a function, call it count times to get count different characters
           (for/list ([i (in-range count)])
             (char-or-func))
           ;; If char-or-func is a character, repeat it count times
           (make-list count char-or-func)))]
    [(and quantifier (eq? quantifier 'optional)) ; ?
     (if (zero? (random 2))
         '()  ; empty list means don't add anything
         (if (procedure? char-or-func)
             ;; If char-or-func is a function, call it once to get a character
             (list (char-or-func))
             ;; If char-or-func is a character, use it as is
             (list char-or-func)))]
    [else
     (if (procedure? char-or-func)
         ;; If char-or-func is a function, call it once to get a character
         (list (char-or-func))
         ;; If char-or-func is a character, use it as is
         (list char-or-func))]))  ; no quantifier, just return as single element list

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
              (let ([char-func (lambda () (cc:vector-random-ref options))])
                (let ([chars (apply-quantifier char-func (token-quantifier token))])
                  (loop (cdr tokens) (append (reverse chars) result)))))]
           [(word-char)
            (let ([char-func (lambda () (cc:random-word-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(whitespace-char)
            (let ([char-func (lambda () (cc:random-whitespace-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(non-whitespace-char)
            (let ([char-func (lambda () (cc:random-non-whitespace-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(non-word-char)
            (let ([char-func (lambda () (cc:random-non-word-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(digit-char)
            (let ([char-func (lambda () (cc:random-digit-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(non-digit-char)
            (let ([char-func (lambda () (cc:random-non-digit-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(any)
            (let ([char-func (lambda () (cc:random-character))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(unicode-property)
            (let* ([property (token-content token)]
                   [char-func (lambda ()
                                (let ([chars (cc:unicode-property-chars property)])
                                  (if (null? chars)
                                      #\? ; Default character if no chars
                                      (list-ref chars (random (length chars))))))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result))))]
           [(group)
            (let* ([group-pattern (token-content token)]
                   [alternatives (string-split group-pattern #rx"\\|")])  ; Split by | to get alternatives
              (if (= (length alternatives) 1)
                  ;; If no | in the group, just process as before
                  (let* ([sub-tokens (tokenize-pattern group-pattern)]
                         [sub-string (generate-from-tokens sub-tokens)])
                    (let ([chars (apply-quantifier sub-string (token-quantifier token))])
                      (loop (cdr tokens) (append (reverse (string->list (if (string? chars) chars (string-join chars "")))) result))))
                  ;; If there are alternatives, pick one randomly
                  (let* ([selected-alternative (list-ref alternatives (random (length alternatives)))]
                         [sub-tokens (tokenize-pattern selected-alternative)]
                         [sub-string (generate-from-tokens sub-tokens)])
                    (let ([chars (apply-quantifier sub-string (token-quantifier token))])
                      (loop (cdr tokens) (append (reverse (string->list (if (string? chars) chars (string-join chars "")))) result))))))]
           [else
            (loop (cdr tokens) result)]
           ))])))
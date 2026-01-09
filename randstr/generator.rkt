#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random
         racket/math)

(require
  "tokenizer.rkt"
  "config.rkt"
  (prefix-in cc: "char-classes.rkt"))

(provide
 (contract-out
  [generate-from-tokens ((listof (struct/c token any/c any/c any/c)) . -> . string?)]))

;; Split group content into top-level alternatives.
;; Only splits on an unescaped '|' when not inside (...) or [...].
(define (split-top-level-alternatives s)
  (define chars (string->list s))
  (define parts '())
  (define current '())
  (define paren-depth 0)
  (define bracket-depth 0)
  (define escaped? #f)
  (define (flush!)
    (set! parts (cons (list->string (reverse current)) parts))
    (set! current '()))
  (for ([c (in-list chars)])
    (cond
      [escaped?
       (set! current (cons c current))
       (set! escaped? #f)]
      [(char=? c #\\)
       (set! current (cons c current))
       (set! escaped? #t)]
      [(char=? c #\[)
       (set! current (cons c current))
       (set! bracket-depth (+ bracket-depth 1))]
      [(char=? c #\])
       (set! current (cons c current))
       (set! bracket-depth (max 0 (- bracket-depth 1)))]
      [(char=? c #\()
       (set! current (cons c current))
       (set! paren-depth (+ paren-depth 1))]
      [(char=? c #\))
       (set! current (cons c current))
       (set! paren-depth (max 0 (- paren-depth 1)))]
      [(and (char=? c #\|) (= paren-depth 0) (= bracket-depth 0))
       (flush!)]
      [else
       (set! current (cons c current))]))
  (flush!)
  (reverse parts))

;; Generate a normal distribution sample by averaging multiple uniform samples
;; order = number of uniform samples to average (central limit theorem)
;; For order 2: average of 2 samples, variance = original/2
;; For order 3: average of 3 samples, variance = original/3, etc.
(define (normal-sample mean order)
  (let ([samples (for/list ([i (in-range order)])
                   (random))])
    ;; Sum of uniform samples centered around 0.5
    ;; Scale to have mean 0, then scale to target mean
    (let* ([sum (apply + samples)]
           [avg (/ sum order)]
           ;; avg is centered around 0.5 with reduced variance
           ;; Convert to a multiplier: 0.5 + deviation
           ;; The deviation range is approximately [-0.5, 0.5] for low orders
           ;; We want the result to be centered around mean
           [deviation (- avg 0.5)]
           ;; Scale deviation based on order (higher order = more concentrated)
           [scaled-deviation (* deviation (sqrt (/ 3.0 order)))]
           ;; Result centered around mean, with some variance
           [result (inexact->exact (round (* mean (+ 1.0 scaled-deviation))))])
      (max 1 result))))

;; Generate a normal distribution sample within a range [min-val, max-val]
;; Uses the same central limit theorem approach but maps to the specified range
(define (normal-range-sample min-val max-val order)
  (let ([samples (for/list ([i (in-range order)])
                   (random))])
    ;; Average of samples gives us a value centered around 0.5
    (let* ([sum (apply + samples)]
           [avg (/ sum order)]
           ;; Map [0, 1] range to [min-val, max-val]
           [range (- max-val min-val)]
           [result (inexact->exact (round (+ min-val (* avg range))))])
      (max min-val (min max-val result)))))

;; Apply quantifier to a character or list of characters
(define (apply-quantifier char-or-func quantifier)
  (cond
    ;; Handle range normal distribution: (list 'normal-range min max order)
    [(and quantifier (list? quantifier) (eq? (car quantifier) 'normal-range))
     (let* ([min-val (cadr quantifier)]
            [max-val (caddr quantifier)]
            [order (cadddr quantifier)]
            [count (normal-range-sample min-val max-val order)])
       (if (procedure? char-or-func)
           (for/list ([i (in-range count)])
             (char-or-func))
           (make-list count char-or-func)))]
    ;; Handle normal distribution quantifier: (list 'normal n order)
    [(and quantifier (list? quantifier) (eq? (car quantifier) 'normal))
     (let* ([mean (cadr quantifier)]
            [order (caddr quantifier)]
            [count (normal-sample mean order)])
       (if (procedure? char-or-func)
           (for/list ([i (in-range count)])
             (char-or-func))
           (make-list count char-or-func)))]
    [(and quantifier (number? quantifier)) ; {n}
     (if (procedure? char-or-func)
         ;; If char-or-func is a function, call it n times to get n different characters
         (for/list ([i (in-range quantifier)])
           (char-or-func))
         ;; If char-or-func is a character, repeat it n times
         (make-list quantifier char-or-func))]
    [(and quantifier (eq? quantifier 'star)) ; *
     (let* ([max-repeat (randstr-max-repeat)]
            [count (random (+ max-repeat 1))])
       (if (procedure? char-or-func)
           ;; If char-or-func is a function, call it count times to get count different characters
           (for/list ([i (in-range count)])
             (char-or-func))
           ;; If char-or-func is a character, repeat it count times
           (make-list count char-or-func)))]
    [(and quantifier (eq? quantifier 'plus)) ; +
     (let* ([max-repeat (randstr-max-repeat)]
            [count (+ 1 (random (max 1 max-repeat)))])
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

;; Generate string from tokens (public API, initializes empty environment)
(define (generate-from-tokens tokens)
  (let-values ([(result env) (generate-from-tokens-with-env tokens (make-hash))])
    result))

;; Internal: Generate string from tokens with an environment for named groups
;; Returns (values result-string updated-env)
(define (generate-from-tokens-with-env tokens env)
  (let loop ([tokens tokens]
             [result '()]
             [env env])
    (cond
      [(null? tokens) (values (list->string (reverse result)) env)]
      [else
       (let ([token (car tokens)])
         (case (token-type token)
           [(literal)
            (let ([char (token-content token)])
              (let ([chars (apply-quantifier char (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result) env)))]
           [(char-class)
            (let ([options (token-content token)])
              (let ([char-func (lambda () (cc:vector-random-ref options))])
                (let ([chars (apply-quantifier char-func (token-quantifier token))])
                  (loop (cdr tokens) (append (reverse chars) result) env))))]
           [(word-char)
            (let ([char-func (lambda () (cc:random-word-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result) env)))]
           [(whitespace-char)
            (let ([char-func (lambda () (cc:random-whitespace-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result) env)))]
           [(non-whitespace-char)
            (let ([char-func (lambda () (cc:random-non-whitespace-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result) env)))]
           [(non-word-char)
            (let ([char-func (lambda () (cc:random-non-word-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result) env)))]
           [(digit-char)
            (let ([char-func (lambda () (cc:random-digit-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result) env)))]
           [(non-digit-char)
            (let ([char-func (lambda () (cc:random-non-digit-char))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result) env)))]
           [(any)
            (let ([char-func (lambda () (cc:random-character))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result) env)))]
           [(unicode-property)
            (let* ([property (token-content token)]
                   [char-func (lambda ()
                                (let ([chars (cc:unicode-property-chars property)])
                                  (if (null? chars)
                                      #\? ; Default character if no chars
                                      (list-ref chars (random (length chars))))))])
              (let ([chars (apply-quantifier char-func (token-quantifier token))])
                (loop (cdr tokens) (append (reverse chars) result) env)))]
           [(group)
            (let* ([group-pattern (token-content token)]
                   [alternatives (split-top-level-alternatives group-pattern)])
              (if (= (length alternatives) 1)
                  ;; If no | in the group, just process as before but ensure it's a function to allow regeneration
                  (let ([char-func (lambda ()
                                     (let* ([sub-tokens (tokenize-pattern group-pattern)])
                                       (let-values ([(sub-string _) (generate-from-tokens-with-env sub-tokens env)])
                                         sub-string)))])
                    (let ([chars (apply-quantifier char-func (token-quantifier token))])
                      (loop (cdr tokens) (append (reverse (string->list (if (string? chars) chars (string-join chars "")))) result) env)))
                  ;; If there are alternatives, we need to handle quantifiers properly
                  ;; For each repetition, we should randomly select an alternative
                  (let ([char-func (lambda ()
                                     (let* ([selected-alternative (list-ref alternatives (random (length alternatives)))]
                                            [sub-tokens (tokenize-pattern selected-alternative)])
                                       (let-values ([(sub-string _) (generate-from-tokens-with-env sub-tokens env)])
                                         sub-string)))])
                    (let ([chars (apply-quantifier char-func (token-quantifier token))])
                      (loop (cdr tokens) (append (reverse (string->list (if (string? chars) chars (string-join chars "")))) result) env)))))]
           [(named-group)
            ;; Named group: content is (cons name pattern)
            ;; Generate the string and store it in the environment
            (let* ([name (car (token-content token))]
                   [group-pattern (cdr (token-content token))]
                   [sub-tokens (tokenize-pattern group-pattern)])
              (let-values ([(sub-string new-env) (generate-from-tokens-with-env sub-tokens env)])
                ;; Store the generated string in environment
                (hash-set! new-env name sub-string)
                ;; Apply quantifier if present (for named groups, quantifier applies to the whole result)
                (let ([char-func (lambda () sub-string)])
                  (let ([chars (apply-quantifier char-func (token-quantifier token))])
                    (loop (cdr tokens) 
                          (append (reverse (string->list (if (string? chars) chars (string-join chars "")))) result) 
                          new-env)))))]
           [(backreference)
            ;; Backreference: content is the name
            ;; Look up the name in the environment and use that string
            (let* ([name (token-content token)]
                   [stored-string (hash-ref env name "")])
              ;; Apply quantifier to the stored string
              (let ([char-func (lambda () stored-string)])
                (let ([chars (apply-quantifier char-func (token-quantifier token))])
                  (loop (cdr tokens) 
                        (append (reverse (string->list (if (string? chars) chars (string-join chars "")))) result) 
                        env))))]
           [else
            (loop (cdr tokens) result env)]))])))

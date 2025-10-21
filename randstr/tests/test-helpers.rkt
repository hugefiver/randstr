#lang racket/base

(require racket/match
         "../tokenizer.rkt"
         "../generator.rkt")

(provide make-regex-checker
         make-pattern-checker)

(define (make-regex-checker generate-fn pattern)
  (lambda ()
    (let ([result (generate-fn)])
      (and (string? result)
           (not (equal? (regexp-match (pregexp pattern) result) #f))))))

(define (make-pattern-checker pattern)
  (let* ([tokens (tokenize-pattern pattern)]
         [checker (make-regex-checker (lambda () (generate-from-tokens tokens)) pattern)])
    (lambda () (checker))))
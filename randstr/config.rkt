#lang racket/base

(require racket/contract)

(provide
 (contract-out
  ;; Maximum repetition for '*' and '+' quantifiers.
  ;; '*' picks an integer in [0, max], '+' picks an integer in [1, max].
  [randstr-max-repeat (parameter/c exact-positive-integer?)]))

(define randstr-max-repeat
  (make-parameter
   5
   (lambda (v)
     (cond
       [(and (exact-integer? v) (positive? v)) v]
       [else (raise-argument-error 'randstr-max-repeat "exact-positive-integer?" v)]))))

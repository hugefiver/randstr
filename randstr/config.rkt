#lang racket/base

(require racket/contract
         racket/random)

(provide
 (contract-out
  ;; Maximum repetition for '*' and '+' quantifiers.
  ;; '*' picks an integer in [0, max], '+' picks an integer in [1, max].
  [randstr-max-repeat (parameter/c exact-positive-integer?)]
  ;; When #t, use cryptographically secure random number generation.
  ;; Default is #f (use standard PRNG for better performance).
  [randstr-secure-random? (parameter/c boolean?)]
  ;; Generate a random non-negative integer less than n.
  ;; Uses crypto-random-bytes when randstr-secure-random? is #t.
  [randstr-random (exact-positive-integer? . -> . exact-nonnegative-integer?)]
  ;; Generate a random floating point number in [0, 1).
  ;; Uses crypto-random-bytes when randstr-secure-random? is #t.
  [randstr-random-real (-> (and/c real? (>=/c 0) (</c 1)))]))

(define randstr-max-repeat
  (make-parameter
   5
   (lambda (v)
     (cond
       [(and (exact-integer? v) (positive? v)) v]
       [else (raise-argument-error 'randstr-max-repeat "exact-positive-integer?" v)]))))

;; Parameter to enable cryptographically secure random number generation
(define randstr-secure-random?
  (make-parameter #f))

;; 2^64 - used for converting 8 bytes to a floating point number in [0, 1)
(define 2^64 (expt 2 64))

;; Calculate the number of bytes needed to represent values up to n
;; Uses ceiling to ensure we have enough bytes for the full range
(define (bytes-needed-for n)
  (max 4 (ceiling (/ (+ 1 (integer-length n)) 8))))

;; Generate a random integer in [0, n) using crypto-random-bytes with rejection sampling
;; This ensures unbiased results by rejecting values that would cause modulo bias
(define (crypto-random-integer n)
  (let* ([byte-count (bytes-needed-for n)]
         ;; Maximum value that can be represented with byte-count bytes
         [max-val (expt 256 byte-count)]
         ;; Largest multiple of n that fits in max-val
         ;; We reject values >= limit to avoid modulo bias
         [limit (* n (quotient max-val n))])
    (let loop ()
      (let* ([bytes (crypto-random-bytes byte-count)]
             [val (for/fold ([acc 0]) ([i (in-range byte-count)])
                    (+ (bytes-ref bytes i) (* acc 256)))])
        (if (< val limit)
            (modulo val n)
            ;; Reject and retry to avoid bias
            (loop))))))

;; Generate a random floating-point number in [0, 1) using crypto-random-bytes
(define (crypto-random-real)
  ;; Use 8 bytes (64 bits) for good precision
  (let* ([bytes (crypto-random-bytes 8)]
         [val (for/fold ([acc 0]) ([i (in-range 8)])
                (+ (bytes-ref bytes i) (* acc 256)))])
    ;; Divide by 2^64 to get a value in [0, 1)
    (/ val 2^64)))

;; Generate a random non-negative integer less than n.
;; Uses cryptographically secure random when randstr-secure-random? is #t.
(define (randstr-random n)
  (if (randstr-secure-random?)
      (crypto-random-integer n)
      (random n)))

;; Generate a random floating point number in [0, 1).
;; Uses cryptographically secure random when randstr-secure-random? is #t.
(define (randstr-random-real)
  (if (randstr-secure-random?)
      (crypto-random-real)
      (random)))

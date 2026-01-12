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

;; Constants for byte manipulation
(define 2^64 (expt 2 64))       ; Used for converting 8 bytes to [0, 1) float
(define BITS-PER-BYTE 8)        ; Number of bits per byte
(define VALUES-PER-BYTE 256)    ; Number of distinct values per byte (2^8)
(define MIN-BYTES 4)            ; Minimum bytes for reasonable randomness (32 bits)

;; Calculate the number of bytes needed to represent values up to n
;; Uses ceiling to ensure we have enough bytes for the full range
(define (bytes-needed-for n)
  (max MIN-BYTES (ceiling (/ (+ 1 (integer-length n)) BITS-PER-BYTE))))

;; Generate a random integer in [0, n) using crypto-random-bytes with rejection sampling
;; This ensures unbiased results by rejecting values that would cause modulo bias
(define (crypto-random-integer n)
  (let* ([byte-count (bytes-needed-for n)]
         ;; Maximum value that can be represented with byte-count bytes
         [max-val (expt VALUES-PER-BYTE byte-count)]
         ;; Largest multiple of n that fits in max-val
         ;; We reject values >= limit to avoid modulo bias
         [limit (* n (quotient max-val n))])
    (let loop ()
      (let* ([bytes (crypto-random-bytes byte-count)]
             [val (for/fold ([acc 0]) ([i (in-range byte-count)])
                    (+ (bytes-ref bytes i) (* acc VALUES-PER-BYTE)))])
        (if (< val limit)
            (modulo val n)
            ;; Reject and retry to avoid bias
            (loop))))))

;; Generate a random floating-point number in [0, 1) using crypto-random-bytes
(define (crypto-random-real)
  ;; Use 8 bytes (64 bits) for good precision
  (let* ([bytes (crypto-random-bytes BITS-PER-BYTE)]
         [val (for/fold ([acc 0]) ([i (in-range BITS-PER-BYTE)])
                (+ (bytes-ref bytes i) (* acc VALUES-PER-BYTE)))])
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

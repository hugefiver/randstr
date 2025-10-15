#lang racket/base

(require racket/format
         racket/date
         "../randstr/main.rkt")

;; Performance benchmarking for randstr library
;; This file measures the performance improvements from the optimizations

(define (benchmark-generate-string pattern iterations)
  (let ([start-time (current-milliseconds)])
    (for ([i (in-range iterations)])
      (randstr pattern))
    (let ([end-time (current-milliseconds)])
      (- end-time start-time))))

(define (benchmark-generate-strings pattern count iterations)
  (let ([start-time (current-milliseconds)])
    (for ([i (in-range iterations)])
      (randstr* pattern count))
    (let ([end-time (current-milliseconds)])
      (- end-time start-time))))

;; Run benchmarks
(printf "Performance Benchmarks for randstr:\n\n")

(printf "Generating single strings 10,000 times:\n")
(printf "Pattern: \"abc\": ~a ms\n" (benchmark-generate-string "abc" 10000))
(printf "Pattern: \"[a-z]{5}\": ~a ms\n" (benchmark-generate-string "[a-z]{5}" 10000))
(printf "Pattern: \"[A-Za-z0-9]{10}\": ~a ms\n" (benchmark-generate-string "[A-Za-z0-9]{10}" 10000))
(printf "Pattern: \"(abc|def){2}\": ~a ms\n" (benchmark-generate-string "(abc|def){2}" 10000))

(printf "\nGenerating multiple strings 1,000 times:\n")
(printf "Pattern: \"[a-z]{3}\" count: 5: ~a ms\n" (benchmark-generate-strings "[a-z]{3}" 5 1000))
(printf "Pattern: \"[A-Z]{4}\" count: 10: ~a ms\n" (benchmark-generate-strings "[A-Z]{4}" 10 1000))

(printf "\nBenchmark complete.\n")
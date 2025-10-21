#lang racket/base

(require "../main.rkt")

;; Benchmark tests for performance evaluation
(printf "Benchmarking randstr performance...\n")

;; Test basic string generation performance
(time
 (for ([i (in-range 1000)])
   (randstr "abc[0-9]{3}def")))

;; Test complex pattern performance
(time
 (for ([i (in-range 500)])
   (randstr "[[:alpha:]]{5}[[:digit:]]{3}[[:punct:]]{2}")))

;; Test multiple string generation performance
(time
 (randstr* "[a-z]{10}" 100))

(printf "Benchmarking completed.\n")
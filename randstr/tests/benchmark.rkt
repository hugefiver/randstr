#lang racket/base

(require "../char-classes.rkt")

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

;; Test Unicode property performance (cached vs uncached)
(printf "Testing Unicode property performance...\n")
;; Clear cache first
(hash-clear! (get-unicode-property-cache))

;; Test uncached performance
(time
 (for ([i (in-range 100)])
   (unicode-property-chars "L")))

;; Test cached performance
(time
 (for ([i (in-range 100)])
   (unicode-property-chars "L")))

;; Test other Unicode property types performance
(printf "Testing other Unicode property performance...\n")
;; Clear cache first
(hash-clear! (get-unicode-property-cache))

;; Test uncached performance for script property
(time
 (for ([i (in-range 50)])
   (unicode-property-chars "Script=Han")))

;; Test cached performance for script property
(time
 (for ([i (in-range 100)])
   (unicode-property-chars "Script=Han")))

;; Test uncached performance for block property
(time
 (for ([i (in-range 50)])
   (unicode-property-chars "Block=Basic_Latin")))

;; Test cached performance for block property
(time
 (for ([i (in-range 100)])
   (unicode-property-chars "Block=Basic_Latin")))

;; Test uncached performance for binary property
(time
 (for ([i (in-range 50)])
   (unicode-property-chars "Alphabetic")))

;; Test cached performance for binary property
(time
 (for ([i (in-range 100)])
   (unicode-property-chars "Alphabetic")))

;; Test full generation performance with Unicode properties
(printf "Testing full generation performance with Unicode properties...\n")
(time
 (for ([i (in-range 100)])
   (randstr "\\p{L}\\p{N}\\p{P}")))

(time
 (for ([i (in-range 100)])
   (randstr "\\p{Script=Latin}\\p{Block=Basic_Latin}")))

(printf "Benchmarking completed.\n")
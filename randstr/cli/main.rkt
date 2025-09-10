#lang racket/base

(require racket/cmdline
         "../main.rkt")

(define pattern (make-parameter ""))
(define count (make-parameter 1))

(define help-text
  "Usage: rrstr [options] <pattern>
Generate random strings based on regex-like patterns.

Options:
  -n, --count N    Generate N strings (default: 1)
  -h, --help       Show this help message
")

(define (show-help)
  (display help-text)
  (exit))

(define (main)
  (command-line
   #:program "randstr"
   #:once-each
   [("-n" "--count") N "Generate N strings"
                     (count (string->number N))]
  ;;;  [("-h" "--help") "Show help"
  ;;;                   (show-help)]
   #:args (pattern-arg)
   (pattern pattern-arg))
  
  (if (= (count) 1)
      (displayln (randstr (pattern)))
      (for ([str (in-list (randstr* (pattern) (count)))])
        (displayln str))))

(main)
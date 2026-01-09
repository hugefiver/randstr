#lang racket/base

(require racket/cmdline
         "../main.rkt")

(provide main)

(define pattern (make-parameter ""))
(define count (make-parameter 1))
(define max-repeat (make-parameter #f))

(define (env->positive-integer name)
  (define v (getenv name))
  (cond
    [(or (not v) (string=? v "")) #f]
    [else
     (define n (string->number v))
     (and (exact-integer? n) (positive? n) n)]))

(define help-text
  "Usage: randstr [options] <pattern>
Generate random strings based on regex-like patterns.

Options:
  -n, --count N    Generate N strings (default: 1)
  -m, --max-repeat N  Maximum repetition for * and + (default: env RANDSTR_MAX_REPEAT or 5)
  -h, --help       Show this help message
")

(define (show-help)
  (display help-text)
  (exit))

(define (main)
  ;; Apply env default (if present) before parsing CLI flags.
  (define env-max (env->positive-integer "RANDSTR_MAX_REPEAT"))
  (when env-max
    (randstr-max-repeat env-max))

  (command-line
   #:program "randstr"
   #:once-each
   [("-n" "--count") N "Generate N strings"
                     (count (string->number N))]
   [("-m" "--max-repeat") N "Maximum repetition for * and +"
                           (max-repeat (string->number N))]
  ;;;  [("-h" "--help") "Show help"
  ;;;                   (show-help)]
   #:args (pattern-arg)
   (pattern pattern-arg))

  (when (max-repeat)
    (randstr-max-repeat (max-repeat)))
  
  (if (= (count) 1)
      (displayln (randstr (pattern)))
      (for ([str (in-list (randstr* (pattern) (count)))])
        (displayln str))))

(module+ main (main))

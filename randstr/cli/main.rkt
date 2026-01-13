#lang racket/base

(require racket/cmdline
         "../main.rkt")

(provide main)

(define pattern (make-parameter ""))
(define count (make-parameter 1))
(define max-repeat (make-parameter #f))
(define secure-random (make-parameter #f))

(define (env->positive-integer name)
  (define v (getenv name))
  (cond
    [(or (not v) (string=? v "")) #f]
    [else
     (define n (string->number v))
     (and (exact-integer? n) (positive? n) n)]))

(define (env->boolean name)
  (define v (getenv name))
  (cond
    [(or (not v) (string=? v "")) #f]
    [else
     (member (string-downcase v) '("1" "true" "yes" "on"))]))

(define help-text
  "Usage: randstr [options] <pattern>
Generate random strings based on regex-like patterns.

Options:
  -n, --count N    Generate N strings (default: 1)
  -m, --max-repeat N  Maximum repetition for * and + (default: env RANDSTR_MAX_REPEAT or 5)
  -s, --secure     Use cryptographically secure random number generator (default: env RANDSTR_SECURE or false)
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
  
  ;; Apply env default for secure random (if present)
  (when (env->boolean "RANDSTR_SECURE")
    (secure-random #t))

  (command-line
   #:program "randstr"
   #:once-each
   [("-n" "--count") N "Generate N strings"
                     (count (string->number N))]
   [("-m" "--max-repeat") N "Maximum repetition for * and +"
                           (max-repeat (string->number N))]
   [("-s" "--secure") "Use cryptographically secure random number generator"
                      (secure-random #t)]
  ;;;  [("-h" "--help") "Show help"
  ;;;                   (show-help)]
   #:args (pattern-arg)
   (pattern pattern-arg))

  (when (max-repeat)
    (randstr-max-repeat (max-repeat)))
  
  (when (secure-random)
    (randstr-secure-random? #t))
  
  (if (= (count) 1)
      (displayln (randstr (pattern)))
      (for ([str (in-list (randstr* (pattern) (count)))])
        (displayln str))))

(module+ main (main))

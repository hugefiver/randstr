#lang racket/base

(require rackunit)
(require racket/list)
(require racket/path)

(require
  "../main.rkt"
  "../generator.rkt"
  "./test-helpers.rkt")

(define (check pattern)
  ((make-pattern-checker pattern)))

;; Test that the optimized implementation produces the same results as expected

(test-case "randstr basic functionality"
           (check-true (check "abc"))

           (let ([result (randstr "[a-z]{3}")])
             (check-equal? (string-length result) 3)
             (check-true (check "[a-z]{3}"))
             (for ([char (string->list result)])
               (check-true (char>=? char #\a))
               (check-true (char<=? char #\z))))

           (let ([results (randstr* "[0-9]{2}" 5)])
             (check-equal? (length results) 5)
             (for ([result results])
               (check-equal? (string-length result) 2)
               (check-true (check "[0-9]{2}"))
               (for ([char (string->list result)])
                 (check-true (char>=? char #\0))
                 (check-true (char<=? char #\9))))))

(test-case "character class with quantifier"
           (let ([result (randstr "[a-z]{5}")])
             (check-equal? (string-length result) 5)
             (check-true (check "[a-z]{5}"))))

;; Test cases to verify random selection behavior in group expressions with quantifiers
(test-case "generate-from-tokens: random selection in group expressions"
           (let* ([pattern "(abc|def){42}"]
                  [expected-length 126]  ; 42 repetitions * 3 characters per alternative
                  [num-tests 50]  ; Sample size for randomness verification
                  [results (for/list ([i (in-range num-tests)])
                             (let ([tokens (tokenize-pattern pattern)])
                               (generate-from-tokens tokens)))])
             ;; Check that all results have the correct length (42 * 3 = 126 characters)
             (for-each (lambda (result)
                         (check-equal? (string-length result) expected-length
                                       (format "Generated string '~a' should have length ~a" result expected-length)))
                       results)

             ;; Check that each result only contains valid alternatives by direct grouping
             (for-each (lambda (result)
                         (let* ([chars (string->list result)]
                                [groups (let loop ([chars chars] [current-group '()] [groups '()])
                                          (if (null? chars)
                                              (if (null? current-group)
                                                  (reverse groups)
                                                  (reverse (cons (reverse current-group) groups)))
                                              (let ([new-group (cons (car chars) current-group)])
                                                (if (= (length new-group) 3)
                                                    (loop (cdr chars) '() (cons (reverse new-group) groups))
                                                    (loop (cdr chars) new-group groups)))))]
                                [valid-groups (filter (lambda (group)
                                                        (or (equal? group '(#\a #\b #\c))
                                                            (equal? group '(#\d #\e #\f))))
                                                      groups)])
                           (check-equal? (length groups) (length valid-groups)
                                         (format "Generated string '~a' should only contain valid alternatives 'abc' or 'def'" result))))
                       results)

             ;; Calculate unique results to verify randomness
             (let* ([unique-results (remove-duplicates results)]
                    [unique-count (length unique-results)])
               ;; With 2^42 possible combinations and only 50 samples, probability of all same results is 1/(2^42)^49 = 1/2^(42*49)
               (check-true (> unique-count 1)
                           (format "Random selection should produce different results; got ~a unique out of ~a attempts. Probability of all same results: ~a"
                                   unique-count num-tests (/ 1 (expt 2 (* 42 49))))))))

;; Run all tests
(void)
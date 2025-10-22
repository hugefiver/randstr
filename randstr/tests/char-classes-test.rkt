#lang racket/base

(require rackunit
         "../char-classes.rkt")

;; Test cases for char-classes module

(test-case "random-character: generates valid character"
  (let ([char (random-character)])
    (check-true (char? char))
    (check-true (or (char-alphabetic? char)
                    (char-numeric? char)))))

(test-case "random-word-char: generates valid word character"
  (let ([char (random-word-char)])
    (check-true (char? char))
    (check-true (or (char-alphabetic? char)
                    (char-numeric? char)
                    (char=? char #\_)))))

(test-case "random-whitespace-char: generates valid whitespace character"
  (let ([char (random-whitespace-char)])
    (check-true (char? char))
    (check-true (or (char=? char #\space)
                    (char=? char #\tab)
                    (char=? char #\newline)
                    (char=? char #\return)))))

(test-case "random-digit-char: generates valid digit character"
  (let ([char (random-digit-char)])
    (check-true (char? char))
    (check-true (char-numeric? char))))

(test-case "random-ref: returns element from list"
  (let ([lst '(a b c d e)])
    (let ([result (random-ref lst)])
      (check-not-false (member result lst)))))

(test-case "character lists: contain expected characters"
  (let ([alpha-chars (alphabetic-chars)])
    (check-not-false (member #\a alpha-chars))
    (check-not-false (member #\Z alpha-chars)))
  (let ([num-chars (numeric-chars)])
    (check-not-false (member #\0 num-chars))
    (check-not-false (member #\9 num-chars)))
  (let ([upper-chars (uppercase-chars)])
    (check-not-false (member #\A upper-chars)))
  (let ([lower-chars (lowercase-chars)])
    (check-not-false (member #\z lower-chars)))
  (let ([blank-chars (blank-chars)])
    (check-not-false (member #\space blank-chars))
    (check-not-false (member #\tab blank-chars))))

(test-case "non-word-chars: contains expected non-word characters"
  (let ([non-word (non-word-chars)])
    (check-not-false (member #\! non-word))
    (check-not-false (member #\@ non-word))
    (check-not-false (member #\# non-word))))

(test-case "hex-digit-chars: contains all hex digits"
  (let ([hex-chars (hex-digit-chars)])
    (check-not-false (member #\0 hex-chars))
    (check-not-false (member #\9 hex-chars))
    (check-not-false (member #\A hex-chars))
    (check-not-false (member #\F hex-chars))
    (check-not-false (member #\a hex-chars))
    (check-not-false (member #\f hex-chars))))

(test-case "unicode-property-chars: returns valid character lists"
  (let ([letter-chars (unicode-property-chars "L")])
    (check-not-false (list? letter-chars))
    (check-true (ormap char-alphabetic? letter-chars)))
  (let ([digit-chars (unicode-property-chars "N")])
    (check-not-false (list? digit-chars))
    (check-true (ormap char-numeric? digit-chars)))
  (let ([punct-chars (unicode-property-chars "P")])
    (check-not-false (list? punct-chars))
    (check-true (ormap char-punctuation? punct-chars)))
  (let ([upper-chars (unicode-property-chars "Lu")])
    (check-not-false (list? upper-chars))
    (check-true (ormap char-upper-case? upper-chars)))
  (let ([lower-chars (unicode-property-chars "Ll")])
    (check-not-false (list? lower-chars))
    (check-true (ormap char-lower-case? lower-chars))))

(test-case "unicode-property-chars: handles invalid property names"
  (check-exn exn:fail? (lambda () (unicode-property-chars "InvalidProperty"))))

(test-case "unicode-property-chars: script properties"
  (let ([han-chars (unicode-property-chars "Script=Han")])
    (check-not-false (list? han-chars))
    (check-true (> (length han-chars) 0))
    (check-true (member #\u4e00 han-chars))) ; 一
  (let ([latin-chars (unicode-property-chars "Script=Latin")])
    (check-not-false (list? latin-chars))
    (check-true (> (length latin-chars) 0))
    (check-true (member #\A latin-chars))))

(test-case "unicode-property-chars: block properties"
  (let ([basic-latin-chars (unicode-property-chars "Block=Basic_Latin")])
    (check-not-false (list? basic-latin-chars))
    (check-true (> (length basic-latin-chars) 0))
    (check-true (member #\A basic-latin-chars)))
  (let ([cjk-unified-chars (unicode-property-chars "Block=CJK_Unified_Ideographs")])
    (check-not-false (list? cjk-unified-chars))
    (check-true (> (length cjk-unified-chars) 0))
    (check-true (member #\u4e00 cjk-unified-chars)))) ; 一

(test-case "unicode-property-chars: binary properties"
  (let ([alphabetic-chars (unicode-property-chars "Alphabetic")])
    (check-not-false (list? alphabetic-chars))
    (check-true (> (length alphabetic-chars) 0))
    (check-true (member #\A alphabetic-chars)))
  (let ([uppercase-chars (unicode-property-chars "Uppercase")])
    (check-not-false (list? uppercase-chars))
    (check-true (> (length uppercase-chars) 0))
    (check-true (member #\A uppercase-chars)))
  (let ([lowercase-chars (unicode-property-chars "Lowercase")])
    (check-not-false (list? lowercase-chars))
    (check-true (> (length lowercase-chars) 0))
    (check-true (member #\a lowercase-chars)))
  (let ([whitespace-chars (unicode-property-chars "White_Space")])
    (check-not-false (list? whitespace-chars))
    (check-true (> (length whitespace-chars) 0))
    (check-true (member #\space whitespace-chars)))
  (let ([hex-digit-chars (unicode-property-chars "Hex_Digit")])
    (check-not-false (list? hex-digit-chars))
    (check-true (> (length hex-digit-chars) 0))
    (check-true (member #\A hex-digit-chars)))
  (let ([ideographic-chars (unicode-property-chars "Ideographic")])
    (check-not-false (list? ideographic-chars))
    (check-true (> (length ideographic-chars) 0))
    (check-true (member #\u4e00 ideographic-chars)))) ; 一

;; Run tests
(void)
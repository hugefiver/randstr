#lang info

(define collection "randstr")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/randstr.scrbl" ())))
(define pkg-desc "A library for generating random strings from regex-like patterns")
(define version "0.1")
(define pkg-authors '(your-name))
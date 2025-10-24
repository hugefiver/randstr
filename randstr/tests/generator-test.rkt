#lang racket/base

(require rackunit
         "../generator.rkt"
         "../tokenizer.rkt"
         "../char-classes.rkt"
         "./test-helpers.rkt")

(define (check pattern)
  (case (substring pattern 0 (min 2 (string-length pattern)))
    [("\\p")
     (let* ([property-name (regexp-replace #px"\\\\p\\{(.*)\\}" pattern "\\1")])
       (check-unicode-property-generate pattern property-name)
       )]
    [else
     ;; For non-Unicode property patterns, use the original method
     ((make-pattern-checker pattern))]))

;; New Unicode property validation functions
(define (check-unicode-property-generate pattern property-name)
  (let* ([tokens (tokenize-pattern pattern)]
         [generated (generate-from-tokens tokens)]
         [char (string-ref generated 0)])
    (case property-name
      [("L") (char-in-general-category? char '(lu ll lt lm lo))]
      [("N") (char-in-general-category? char '(nd nl no))]
      [("P") (char-in-general-category? char '(pc pd ps pe pi pf po))]
      [("M") (char-in-general-category? char '(mn mc me))]
      [("C") (char-in-general-category? char '(cc cf cs co cn))]
      [("S") (char-in-general-category? char '(sm sc sk so))]
      [("Lu") (char-in-general-category? char '(lu))]
      [("Ll") (char-in-general-category? char '(ll))]
      [("Lt") (char-in-general-category? char '(lt))]
      [("Lm") (char-in-general-category? char '(lm))]
      [("Lo") (char-in-general-category? char '(lo))]
      [("Nd") (char-in-general-category? char '(nd))]
      [("Nl") (char-in-general-category? char '(nl))]
      [("No") (char-in-general-category? char '(no))]
      [("Pc") (char-in-general-category? char '(pc))]
      [("Pd") (char-in-general-category? char '(pd))]
      [("Ps") (char-in-general-category? char '(ps))]
      [("Pe") (char-in-general-category? char '(pe))]
      [("Pi") (char-in-general-category? char '(pi))]
      [("Pf") (char-in-general-category? char '(pf))]
      [("Po") (char-in-general-category? char '(po))]
      [("Mn") (char-in-general-category? char '(mn))]
      [("Mc") (char-in-general-category? char '(mc))]
      [("Me") (char-in-general-category? char '(me))]
      [("Z") (char-in-general-category? char '(zs zl zp))]
      [("Zs") (char-in-general-category? char '(zs))]
      [("Zl") (char-in-general-category? char '(zl))]
      [("Zp") (char-in-general-category? char '(zp))]
      [("Sm") (char-in-general-category? char '(sm))]
      [("Sc") (char-in-general-category? char '(sc))]
      [("Sk") (char-in-general-category? char '(sk))]
      [("So") (char-in-general-category? char '(so))]
      [("Cc") (char-in-general-category? char '(cc))]
      [("Cf") (char-in-general-category? char '(cf))]
      [("Cs") (char-in-general-category? char '(cs))]
      [("Co") (char-in-general-category? char '(co))]
      [("Cn") (char-in-general-category? char '(cn))]
      [("Alphabetic") (char-alphabetic? char)]
      [("Uppercase") (char-upper-case? char)]
      [("Lowercase") (char-lower-case? char)]
      [("White_Space") (char-whitespace? char)]
      [("Hex_Digit") (char-hex-digit? char)]
      [("Ideographic") (char-ideographic? char)]
      [("ASCII") (<= (char->integer char) 127)]
      [("Any") #t]
      [("Script=Han" "Han" "Hani") (char-in-han-script? char)]
      [("Script=Latin" "Latin" "Latn") (char-in-latin-script? char)]
      [("Script=Greek" "Greek" "Grek") (char-in-greek-script? char)]
      [("Script=Cyrillic" "Cyrillic" "Cyrl") (char-in-cyrillic-script? char)]
      [("Script=Hiragana" "Hiragana" "Hira") (char-in-hiragana-script? char)]
      [("Script=Katakana" "Katakana" "Kana") (char-in-katakana-script? char)]
      [("Block=Basic_Latin") (char-in-basic-latin-block? char)]
      [("Block=Latin-1_Supplement") (char-in-latin-1-supplement-block? char)]
      [("Block=CJK_Unified_Ideographs") (char-in-cjk-unified-ideographs-block? char)]
      [else (char-in-unicode-property? char property-name)])))


;; Test cases for generator module
(test-case "generate-from-tokens: literal characters"
           (check-true (check "abc")))

(test-case "generate-from-tokens: literal characters with anchors"
           (check-true (check "^abc$")))

(test-case "generate-from-tokens: character class"
           (check-true (check "[abc]")))

(test-case "generate-from-tokens: escape sequences"
           (check-true (check "\\d\\w\\s")))

(test-case "generate-from-tokens: quantifiers"
           (check-true (check "a{3}")))

(test-case "generate-from-tokens: star quantifier"
           (check-true (check "a*")))

(test-case "generate-from-tokens: plus quantifier"
           (check-true (check "b+")))

(test-case "generate-from-tokens: optional quantifier"
           (check-true (check "c?")))

(test-case "generate-from-tokens: basic Unicode properties"
           (check-true (check-unicode-property-generate "\\p{L}" "L"))   ; Letter
           (check-true (check-unicode-property-generate "\\p{N}" "N"))   ; Number
           (check-true (check-unicode-property-generate "\\p{P}" "P"))   ; Punctuation
           (check-true (check-unicode-property-generate "\\p{M}" "M"))   ; Mark
           (check-true (check-unicode-property-generate "\\p{C}" "C"))   ; Other
           (check-true (check-unicode-property-generate "\\p{S}" "S")))  ; Symbol

(test-case "generate-from-tokens: script Unicode properties"
           (check-true (check-unicode-property-generate "\\p{Script=Han}" "Script=Han"))      ; Han script
           (check-true (check-unicode-property-generate "\\p{Script=Latin}" "Script=Latin"))    ; Latin script
           (check-true (check-unicode-property-generate "\\p{Script=Cyrillic}" "Script=Cyrillic")) ; Cyrillic script
           (check-true (check-unicode-property-generate "\\p{Script=Arabic}" "Script=Arabic"))   ; Arabic script
           (check-true (check-unicode-property-generate "\\p{Script=Hiragana}" "Script=Hiragana"))) ; Hiragana script

(test-case "generate-from-tokens: block Unicode properties"
           (check-true (check-unicode-property-generate "\\p{Block=Basic_Latin}" "Block=Basic_Latin"))
           (check-true (check-unicode-property-generate "\\p{Block=Latin-1_Supplement}" "Block=Latin-1_Supplement"))
           (check-true (check-unicode-property-generate "\\p{Block=Cyrillic}" "Block=Cyrillic"))
           (check-true (check-unicode-property-generate "\\p{Block=Arabic}" "Block=Arabic"))
           (check-true (check-unicode-property-generate "\\p{Block=Hiragana}" "Block=Hiragana")))

(test-case "generate-from-tokens: binary Unicode properties"
           (check-true (check-unicode-property-generate "\\p{Alphabetic}" "Alphabetic"))
           (check-true (check-unicode-property-generate "\\p{Uppercase}" "Uppercase"))
           (check-true (check-unicode-property-generate "\\p{Lowercase}" "Lowercase"))
           (check-true (check-unicode-property-generate "\\p{White_Space}" "White_Space"))
           (check-true (check-unicode-property-generate "\\p{Hex_Digit}" "Hex_Digit"))
           (check-true (check-unicode-property-generate "\\p{Ideographic}" "Ideographic")))


(test-case "generate-from-tokens: Unicode properties with quantifiers"
           ;;; This test is commented out due to potential empty string generation
           ;;; (check-true (check-unicode-property-generate "\\p{L}*" "L"))

           (check-true (check-unicode-property-generate "\\p{N}+" "N"))
           (check-true (check-unicode-property-generate "\\p{L}{3}" "L"))

           ;;; This test is commented out due to potential empty string generation
           ;;; (check-true (check-unicode-property-generate "\\p{Script=Latin}?" "Script=Latin"))
           )

;; Run tests
(void)

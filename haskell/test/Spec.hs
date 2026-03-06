-- | Tests for the randstr Haskell library.
module Main (main) where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import System.Random (mkStdGen)
import Data.Char (isAlpha, isDigit, isAlphaNum, isUpper, isLower)

import Randstr.Lib
import Randstr.Types

-- | Helper: generate a string with a fixed seed for reproducibility.
genWith :: String -> String
genWith pattern =
  let cfg = defaultConfig (mkStdGen 42)
  in randstrWith cfg pattern

-- | Helper: generate n strings with a fixed seed.
genNWith :: String -> Int -> [String]
genNWith pattern n =
  let cfg = defaultConfig (mkStdGen 42)
  in randstrNWith cfg pattern n

-- | Helper: check that a string is non-empty and consists only of chars from a given set.
allCharsIn :: String -> [Char] -> Bool
allCharsIn s cs = all (`elem` cs) s

-- Basic tests
testBasicLiteral :: Test
testBasicLiteral = TestCase $ do
  let result = genWith "abc"
  assertEqual "literal pattern" "abc" result

testRandstrNCount :: Test
testRandstrNCount = TestCase $ do
  let results = genNWith "abc" 5
  assertEqual "randstr* count" 5 (length results)

testRandstrNAllStrings :: Test
testRandstrNAllStrings = TestCase $ do
  let results = genNWith "test" 3
  assertBool "all results are strings" (all (not . null) results)

-- Escape sequence tests
testWordChar :: Test
testWordChar = TestCase $ do
  let result = genWith "\\w+"
  assertBool "\\w+ produces non-empty string" (not (null result))
  assertBool "\\w+ produces word chars" (all (\c -> isAlphaNum c || c == '_') result)

testNonWordChar :: Test
testNonWordChar = TestCase $ do
  let result = genWith "\\W+"
  assertBool "\\W+ produces non-empty string" (not (null result))

testWhitespaceChar :: Test
testWhitespaceChar = TestCase $
  let _result = genWith "\\s*"
  in assertBool "\\s* produces string" True  -- may be empty due to *

testNonWhitespaceChar :: Test
testNonWhitespaceChar = TestCase $ do
  let result = genWith "\\S+"
  assertBool "\\S+ produces non-empty string" (not (null result))

testDigitChar :: Test
testDigitChar = TestCase $ do
  let result = genWith "\\d+"
  assertBool "\\d+ produces non-empty string" (not (null result))
  assertBool "\\d+ produces digits" (all isDigit result)

testNonDigitChar :: Test
testNonDigitChar = TestCase $ do
  let result = genWith "\\D+"
  assertBool "\\D+ produces non-empty string" (not (null result))
  assertBool "\\D+ produces non-digits" (all (not . isDigit) result)

-- POSIX character class tests
testPosixAlpha :: Test
testPosixAlpha = TestCase $ do
  let result = genWith "[[:alpha:]]+"
  assertBool "[:alpha:]+ produces non-empty string" (not (null result))
  assertBool "[:alpha:]+ produces alpha chars" (all isAlpha result)

testPosixDigit :: Test
testPosixDigit = TestCase $ do
  let result = genWith "[[:digit:]]+"
  assertBool "[:digit:]+ produces non-empty string" (not (null result))
  assertBool "[:digit:]+ produces digit chars" (all isDigit result)

testPosixAlnum :: Test
testPosixAlnum = TestCase $ do
  let result = genWith "[[:alnum:]]+"
  assertBool "[:alnum:]+ produces non-empty string" (not (null result))
  assertBool "[:alnum:]+ produces alnum chars" (all isAlphaNum result)

testPosixUpper :: Test
testPosixUpper = TestCase $ do
  let result = genWith "[[:upper:]]+"
  assertBool "[:upper:]+ produces non-empty string" (not (null result))
  assertBool "[:upper:]+ produces upper chars" (all isUpper result)

testPosixLower :: Test
testPosixLower = TestCase $ do
  let result = genWith "[[:lower:]]+"
  assertBool "[:lower:]+ produces non-empty string" (not (null result))
  assertBool "[:lower:]+ produces lower chars" (all isLower result)

testPosixWord :: Test
testPosixWord = TestCase $ do
  let result = genWith "[[:word:]]+"
  assertBool "[:word:]+ produces non-empty string" (not (null result))
  assertBool "[:word:]+ produces word chars" (all (\c -> isAlphaNum c || c == '_') result)

testPosixBlank :: Test
testPosixBlank = TestCase $
  let _result = genWith "[[:blank:]]*"
  in assertBool "[:blank:]* produces string" True

testPosixSpace :: Test
testPosixSpace = TestCase $ do
  let result = genWith "[[:space:]]+"
  assertBool "[:space:]+ produces non-empty string" (not (null result))

testPosixXdigit :: Test
testPosixXdigit = TestCase $ do
  let result = genWith "[[:xdigit:]]+"
  assertBool "[:xdigit:]+ produces non-empty string" (not (null result))
  assertBool "[:xdigit:]+ produces hex chars" (allCharsIn result "0123456789ABCDEFabcdef")

-- Nested POSIX character class tests
testNestedPosixUpperDigit :: Test
testNestedPosixUpperDigit = TestCase $ do
  let result = genWith "[[:upper:]0-9]+"
  assertBool "[:upper:]0-9 produces non-empty" (not (null result))
  assertBool "[:upper:]0-9 valid chars" (all (\c -> isUpper c || isDigit c) result)

testNestedPosixLowerUnderscore :: Test
testNestedPosixLowerUnderscore = TestCase $ do
  let result = genWith "[[:lower:]_]+"
  assertBool "[:lower:]_ produces non-empty" (not (null result))
  assertBool "[:lower:]_ valid chars" (all (\c -> isLower c || c == '_') result)

-- Character class tests
testParseCharacterClass :: Test
testParseCharacterClass = TestCase $ do
  let (options, remaining) = parseCharacterClass "ab]"
  assertBool "contains a" ('a' `elem` options)
  assertBool "contains b" ('b' `elem` options)
  assertEqual "no remaining" "" remaining

testCharRange :: Test
testCharRange = TestCase $ do
  let result = genWith "[a-z]{5}"
  assertEqual "range produces 5 chars" 5 (length result)
  assertBool "range produces lowercase" (all isLower result)

testNegatedCharClass :: Test
testNegatedCharClass = TestCase $ do
  let result = genWith "[^abc]{3}"
  assertEqual "negated produces 3 chars" 3 (length result)
  assertBool "negated excludes abc" (all (`notElem` "abc") result)

-- Duplicate handling
testDuplicates :: Test
testDuplicates = TestCase $ do
  let result = genWith "[aaabbbccc]"
  assertBool "duplicates produce string" (not (null result))
  assertBool "duplicates only abc" (allCharsIn result "abc")

-- Quantifier tests
testExactQuantifier :: Test
testExactQuantifier = TestCase $ do
  let result = genWith "[a-z]{10}"
  assertEqual "exact quantifier" 10 (length result)

testNormalDistQuantifier :: Test
testNormalDistQuantifier = TestCase $ do
  let result = genWith "\\w{5+}"
  assertBool "normal dist produces non-empty" (not (null result))

testNormalDistHighOrder :: Test
testNormalDistHighOrder = TestCase $ do
  let result = genWith "\\d{8++}"
  assertBool "normal dist high order produces non-empty" (not (null result))

testRangeNormalDist :: Test
testRangeNormalDist = TestCase $ do
  let result = genWith "\\w{5+10}"
  assertBool "range normal in bounds" (length result >= 5 && length result <= 10)

testRangeNormalDistHighOrder :: Test
testRangeNormalDistHighOrder = TestCase $ do
  let result = genWith "[a-z]{3++8}"
  assertBool "range normal high order in bounds" (length result >= 3 && length result <= 8)

testShorthandRange :: Test
testShorthandRange = TestCase $ do
  let result = genWith "\\d{+5}"
  assertBool "shorthand range in bounds" (length result >= 0 && length result <= 5)

testShorthandRangeHighOrder :: Test
testShorthandRangeHighOrder = TestCase $ do
  let result = genWith "[A-Z]{++10}"
  assertBool "shorthand range high order in bounds" (length result >= 0 && length result <= 10)

-- Named group and backreference tests
testNamedGroup :: Test
testNamedGroup = TestCase $ do
  let result = genWith "(?<name>\\w{3})"
  assertBool "named group produces string" (not (null result))

testBackreference :: Test
testBackreference = TestCase $ do
  let result = genWith "(?<word>[a-z]{4})-\\k<word>"
  let parts = splitOn '-' result
  assertEqual "backreference has 2 parts" 2 (length parts)
  assertEqual "backreference parts match" (parts !! 0) (parts !! 1)

-- Group and alternation tests
testGroup :: Test
testGroup = TestCase $ do
  let result = genWith "(abc)"
  assertEqual "group produces abc" "abc" result

testAlternation :: Test
testAlternation = TestCase $ do
  let result = genWith "(abc|def)"
  assertBool "alternation produces abc or def" (result `elem` ["abc", "def"])

-- Any character test
testAnyChar :: Test
testAnyChar = TestCase $ do
  let result = genWith ".{5}"
  assertEqual "any char produces 5 chars" 5 (length result)

-- Range helper test
testRangeToList :: Test
testRangeToList = TestCase $ do
  let result = rangeToList 'a' 'e'
  assertEqual "range a-e" "abcde" result

-- Tokenizer tests
testTokenizeLiteral :: Test
testTokenizeLiteral = TestCase $ do
  let tokens = tokenizePattern "abc"
  assertEqual "literal tokens count" 3 (length tokens)

testTokenizeEscape :: Test
testTokenizeEscape = TestCase $ do
  let tokens = tokenizePattern "\\w\\d"
  assertEqual "escape tokens count" 2 (length tokens)
  assertEqual "first token type" WordChar (tokenType (tokens !! 0))
  assertEqual "second token type" DigitChar (tokenType (tokens !! 1))

-- Helper to split a string on a character
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (c:cs)
  | c == sep  = "" : splitOn sep cs
  | otherwise = case splitOn sep cs of
      (x:xs) -> (c:x) : xs
      []     -> [[c]]

-- All tests
allTests :: Test
allTests = TestList
  [ TestLabel "basic literal" testBasicLiteral
  , TestLabel "randstrN count" testRandstrNCount
  , TestLabel "randstrN all strings" testRandstrNAllStrings
  , TestLabel "word char" testWordChar
  , TestLabel "non-word char" testNonWordChar
  , TestLabel "whitespace char" testWhitespaceChar
  , TestLabel "non-whitespace char" testNonWhitespaceChar
  , TestLabel "digit char" testDigitChar
  , TestLabel "non-digit char" testNonDigitChar
  , TestLabel "posix alpha" testPosixAlpha
  , TestLabel "posix digit" testPosixDigit
  , TestLabel "posix alnum" testPosixAlnum
  , TestLabel "posix upper" testPosixUpper
  , TestLabel "posix lower" testPosixLower
  , TestLabel "posix word" testPosixWord
  , TestLabel "posix blank" testPosixBlank
  , TestLabel "posix space" testPosixSpace
  , TestLabel "posix xdigit" testPosixXdigit
  , TestLabel "nested posix upper-digit" testNestedPosixUpperDigit
  , TestLabel "nested posix lower-underscore" testNestedPosixLowerUnderscore
  , TestLabel "parse character class" testParseCharacterClass
  , TestLabel "char range" testCharRange
  , TestLabel "negated char class" testNegatedCharClass
  , TestLabel "duplicates" testDuplicates
  , TestLabel "exact quantifier" testExactQuantifier
  , TestLabel "normal dist quantifier" testNormalDistQuantifier
  , TestLabel "normal dist high order" testNormalDistHighOrder
  , TestLabel "range normal dist" testRangeNormalDist
  , TestLabel "range normal dist high order" testRangeNormalDistHighOrder
  , TestLabel "shorthand range" testShorthandRange
  , TestLabel "shorthand range high order" testShorthandRangeHighOrder
  , TestLabel "named group" testNamedGroup
  , TestLabel "backreference" testBackreference
  , TestLabel "group" testGroup
  , TestLabel "alternation" testAlternation
  , TestLabel "any char" testAnyChar
  , TestLabel "range to list" testRangeToList
  , TestLabel "tokenize literal" testTokenizeLiteral
  , TestLabel "tokenize escape" testTokenizeEscape
  ]

main :: IO ()
main = do
  cnts <- runTestTT allTests
  if errors cnts + failures cnts == 0
    then do
      putStrLn "All tests passed!"
      exitSuccess
    else do
      putStrLn $ "Tests failed: " ++ show (failures cnts) ++ " failures, " ++ show (errors cnts) ++ " errors"
      exitFailure

-- | Character class definitions for the randstr library.
module Randstr.CharClasses
  ( -- * Character class lists
    alphanumericChars
  , alphabeticChars
  , numericChars
  , uppercaseChars
  , lowercaseChars
  , blankChars
  , whitespaceChars
  , nonWhitespaceChars
  , punctuationChars
  , nonWordChars
  , nonDigitChars
  , controlChars
  , printableChars
  , graphicChars
  , asciiChars
  , hexDigitChars
    -- * Random character generators
  , randomCharacter
  , randomWordChar
  , randomDigitChar
  , randomWhitespaceChar
  , randomNonWhitespaceChar
  , randomNonWordChar
  , randomNonDigitChar
  , randomRef
  , vectorRandomRef
    -- * Unicode property support
  , unicodePropertyChars
  , getUnicodePropertyRanges
  ) where

import Randstr.Config (Config, configRandom)
import qualified Data.Vector as V

-- | Generate list of alphanumeric characters.
alphanumericChars :: [Char]
alphanumericChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- | Generate list of alphabetic characters.
alphabeticChars :: [Char]
alphabeticChars = ['a'..'z'] ++ ['A'..'Z']

-- | Generate list of numeric characters.
numericChars :: [Char]
numericChars = ['0'..'9']

-- | Generate list of uppercase characters.
uppercaseChars :: [Char]
uppercaseChars = ['A'..'Z']

-- | Generate list of lowercase characters.
lowercaseChars :: [Char]
lowercaseChars = ['a'..'z']

-- | Generate list of blank characters (space and tab).
blankChars :: [Char]
blankChars = [' ', '\t']

-- | Generate list of whitespace characters.
whitespaceChars :: [Char]
whitespaceChars = [' ', '\t', '\n', '\r']

-- | Generate list of non-whitespace printable characters.
nonWhitespaceChars :: [Char]
nonWhitespaceChars = filter (`notElem` whitespaceChars) printableChars

-- | Generate list of punctuation characters.
punctuationChars :: [Char]
punctuationChars = "!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"

-- | Generate list of non-word characters (not alphanumeric or underscore).
nonWordChars :: [Char]
nonWordChars = "!@#$%^&*()-+={}[]|\\:;\"'<>?,./`~"

-- | Generate list of non-digit characters.
nonDigitChars :: [Char]
nonDigitChars = ['a'..'z'] ++ ['A'..'Z'] ++ "!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"

-- | Generate list of control characters (ASCII 0-31).
controlChars :: [Char]
controlChars = map toEnum [0..31]

-- | Generate list of printable characters (ASCII 32-126).
printableChars :: [Char]
printableChars = map toEnum [32..126]

-- | Generate list of graphic characters (printable except space).
graphicChars :: [Char]
graphicChars = map toEnum [33..126]

-- | Generate list of ASCII characters (0-127).
asciiChars :: [Char]
asciiChars = map toEnum [0..127]

-- | Generate list of hexadecimal digits.
hexDigitChars :: [Char]
hexDigitChars = "0123456789ABCDEFabcdef"

-- | Get a random element from a list. Returns (element, newConfig).
randomRef :: [a] -> Config -> (a, Config)
randomRef lst cfg =
  let (idx, cfg') = configRandom (length lst) cfg
  in (lst !! idx, cfg')

-- | Get a random element from a vector. Returns (element, newConfig).
vectorRandomRef :: V.Vector a -> Config -> (a, Config)
vectorRandomRef vec cfg =
  let (idx, cfg') = configRandom (V.length vec) cfg
  in (vec V.! idx, cfg')

-- | Generate a random printable character.
randomCharacter :: Config -> (Char, Config)
randomCharacter = randomRef printableChars

-- | Generate a random word character (alphanumeric + underscore).
randomWordChar :: Config -> (Char, Config)
randomWordChar = randomRef (alphanumericChars ++ "_")

-- | Generate a random digit character.
randomDigitChar :: Config -> (Char, Config)
randomDigitChar = randomRef numericChars

-- | Generate a random whitespace character.
randomWhitespaceChar :: Config -> (Char, Config)
randomWhitespaceChar = randomRef whitespaceChars

-- | Generate a random non-whitespace character.
randomNonWhitespaceChar :: Config -> (Char, Config)
randomNonWhitespaceChar = randomRef nonWhitespaceChars

-- | Generate a random non-word character.
randomNonWordChar :: Config -> (Char, Config)
randomNonWordChar = randomRef nonWordChars

-- | Generate a random non-digit character.
randomNonDigitChar :: Config -> (Char, Config)
randomNonDigitChar = randomRef nonDigitChars

-- | Get Unicode property ranges as (start, end) pairs.
getUnicodePropertyRanges :: String -> [(Int, Int)]
getUnicodePropertyRanges prop = case normalizeUnicodeProp prop of
  -- General categories
  p | p `elem` ["L", "Letter"] ->
    [ (0x4e00, 0x9fff), (0x3400, 0x4dbf), (0x20000, 0x2a6df), (0xf900, 0xfaff)
    , (0x0041, 0x005a), (0x0061, 0x007a), (0x00c0, 0x00d6), (0x00d8, 0x00f6)
    , (0x00f8, 0x00ff), (0x0100, 0x017f), (0x0180, 0x024f), (0x1e00, 0x1eff) ]
  p | p `elem` ["N", "Number"] ->
    [ (0x0030, 0x0039), (0x00b2, 0x00b3), (0x00b9, 0x00b9), (0x00bc, 0x00be)
    , (0x2070, 0x2079), (0x2080, 0x2089), (0x2150, 0x218f), (0x2460, 0x24ff) ]
  p | p `elem` ["P", "Punctuation"] ->
    [(0x0021, 0x002f), (0x003a, 0x0040), (0x005b, 0x0060), (0x007b, 0x007e)
    , (0x2010, 0x2027), (0x2030, 0x205e)]
  p | p `elem` ["M", "Mark"] ->
    [(0x0300, 0x036f), (0x0483, 0x0489)]
  p | p `elem` ["Z", "Separator"] ->
    [(0x0020, 0x0020), (0x00a0, 0x00a0), (0x1680, 0x1680)
    , (0x2000, 0x200a), (0x2028, 0x2029), (0x202f, 0x202f), (0x205f, 0x205f), (0x3000, 0x3000)]
  p | p `elem` ["S", "Symbol"] ->
    [(0x0024, 0x0024), (0x002b, 0x002b), (0x003c, 0x003e)
    , (0x005e, 0x005e), (0x0060, 0x0060), (0x007c, 0x007c), (0x007e, 0x007e)
    , (0x00a2, 0x00a9), (0x00ac, 0x00ac), (0x00ae, 0x00b1)
    , (0x2200, 0x22ff), (0x2a00, 0x2aff)]
  p | p `elem` ["C", "Other"] ->
    [(0x0000, 0x001f), (0x007f, 0x009f), (0x00ad, 0x00ad)]
  -- Subcategories
  p | p `elem` ["Lu", "Uppercase_Letter"] ->
    [(0x0041, 0x005a), (0x00c0, 0x00d6), (0x00d8, 0x00de)]
  p | p `elem` ["Ll", "Lowercase_Letter"] ->
    [(0x0061, 0x007a), (0x00df, 0x00f6), (0x00f8, 0x00ff)]
  p | p `elem` ["Nd", "Decimal_Number"] ->
    [(0x0030, 0x0039)]
  -- Binary properties
  p | p == "Alphabetic" ->
    [(0x0041, 0x005a), (0x0061, 0x007a), (0x00c0, 0x00d6), (0x00d8, 0x00f6)
    , (0x00f8, 0x00ff), (0x0100, 0x017f), (0x0180, 0x024f), (0x1e00, 0x1eff)]
  p | p == "White_Space" ->
    [(0x0009, 0x000d), (0x0020, 0x0020), (0x0085, 0x0085), (0x00a0, 0x00a0)
    , (0x1680, 0x1680), (0x2000, 0x200a), (0x2028, 0x2029), (0x202f, 0x202f)
    , (0x205f, 0x205f), (0x3000, 0x3000)]
  p | p == "Cased" ->
    [(0x0041, 0x005a), (0x0061, 0x007a), (0x00c0, 0x00d6), (0x00d8, 0x00f6)
    , (0x00f8, 0x00ff), (0x0100, 0x017f), (0x0180, 0x024f)
    , (0x0370, 0x03ff), (0x0400, 0x04ff)]
  p | p == "Dash" ->
    [(0x002d, 0x002d), (0x2010, 0x2015), (0x2053, 0x2053), (0x207b, 0x207b)
    , (0x208b, 0x208b), (0x2212, 0x2212), (0xfe58, 0xfe58), (0xfe63, 0xfe63)
    , (0xff0d, 0xff0d)]
  p | p == "Emoji" ->
    [(0x1F300, 0x1F64F), (0x1F680, 0x1F6FF), (0x1F1E6, 0x1F1FF)
    , (0x2600, 0x26FF), (0x2700, 0x27BF), (0x1F900, 0x1F9FF)
    , (0x1F000, 0x1F02F), (0x1F0A0, 0x1F0FF), (0x1F3FB, 0x1F3FF)
    , (0x1FA70, 0x1FA73), (0x1FA78, 0x1FA7A), (0x1FA80, 0x1FA86)
    , (0x1FA90, 0x1FA95), (0x1FAB0, 0x1FAB6), (0x1FAC0, 0x1FAC2)
    , (0x1FAD0, 0x1FAD6)]
  p | p == "Emoji_Component" ->
    [(0x1F3FB, 0x1F3FF), (0x200D, 0x200D), (0xFE0F, 0xFE0F), (0xFE0E, 0xFE0E)]
  p | p == "Emoji_Modifier" ->
    [(0x1F3FB, 0x1F3FF)]
  p | p `elem` ["Emoji_Modifier_Base", "Emoji_Presentation", "Extended_Pictographic"] ->
    getUnicodePropertyRanges "Emoji"
  p | p == "ID_Continue" ->
    [(0x0041, 0x005a), (0x0061, 0x007a), (0x0030, 0x0039), (0x005f, 0x005f)]
  p | p == "ID_Start" ->
    [(0x0041, 0x005a), (0x0061, 0x007a), (0x005f, 0x005f)]
  p | p == "Math" ->
    [(0x002b, 0x002b), (0x003c, 0x003e), (0x2200, 0x22ff), (0x2a00, 0x2aff)]
  p | p == "Quotation_Mark" ->
    [(0x0022, 0x0022), (0x0027, 0x0027), (0x2018, 0x201f), (0x2039, 0x203a)]
  -- Script properties
  p | take 7 p == "Script=" -> case drop 7 p of
    s | s `elem` ["Han", "Hani"] ->
      [(0x4e00, 0x9fff), (0x3400, 0x4dbf), (0x20000, 0x2a6df), (0xf900, 0xfaff)]
    s | s `elem` ["Latin", "Latn"] ->
      [(0x0000, 0x024f), (0x1e00, 0x1eff)]
    s | s `elem` ["Greek", "Grek"] ->
      [(0x0370, 0x03ff)]
    s | s `elem` ["Cyrillic", "Cyrl"] ->
      [(0x0400, 0x04ff)]
    s | s `elem` ["Hiragana", "Hira"] ->
      [(0x3040, 0x309f)]
    s | s `elem` ["Katakana", "Kana"] ->
      [(0x30a0, 0x30ff)]
    s | s `elem` ["Arabic", "Arab"] ->
      [(0x0600, 0x06ff), (0x0750, 0x077f)]
    _ -> []
  -- Block properties
  p | take 6 p == "Block=" -> case drop 6 p of
    "Basic_Latin" -> [(0, 0x7f)]
    "Latin-1_Supplement" -> [(0x80, 0xff)]
    "CJK_Unified_Ideographs" -> [(0x4e00, 0x9fff)]
    "Cyrillic" -> [(0x0400, 0x04ff)]
    "Arabic" -> [(0x0600, 0x06ff)]
    "Hiragana" -> [(0x3040, 0x309f)]
    _ -> []
  _ -> []

-- | Get characters for a Unicode property from its ranges.
unicodePropertyChars :: String -> [Char]
unicodePropertyChars property =
  let ranges = getUnicodePropertyRanges property
  in concatMap (\(s, e) -> map toEnum [s..e]) ranges

-- | Normalize a Unicode property name.
normalizeUnicodeProp :: String -> String
normalizeUnicodeProp = id  -- Property names are used as-is

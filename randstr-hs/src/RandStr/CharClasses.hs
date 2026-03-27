-- | Character class definitions and random character generators.
-- Faithfully reimplements char-classes.rkt from the Racket version.
module RandStr.CharClasses
  ( -- * Random character generators
    randomCharacter
  , randomWordChar
  , randomNonWordChar
  , randomWhitespaceChar
  , randomNonWhitespaceChar
  , randomDigitChar
  , randomNonDigitChar
  , arrayRandomRef
    -- * Character lists
  , printableChars
  , alphanumericChars
  , alphabeticChars
  , numericChars
  , nonWordChars
  , nonDigitChars
  , uppercaseChars
  , lowercaseChars
  , blankChars
  , whitespaceChars
  , nonWhitespaceChars
  , punctuationChars
  , controlChars
  , graphicChars
  , asciiChars
  , hexDigitChars
  , wordCharString
    -- * POSIX class lookups
  , posixClassChars
    -- * Unicode property support
  , getUnicodePropertyRanges
  , unicodePropertyCharCount
  , unicodePropertyCharAtIndex
  , normalizeUnicodePropertyName
  ) where

import Data.Char (chr, generalCategory, GeneralCategory(..))
import RandStr.Types (CharArray, charArrayFromList, charArrayLength, charArrayIndex)
import RandStr.Config (Config, randstrRandom)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Character lists (matching Racket's definitions exactly)
-- ---------------------------------------------------------------------------

-- | Printable ASCII characters: 32 (space) through 126 (~).
printableChars :: [Char]
printableChars = [chr 32 .. chr 126]

-- | Graphic characters: 33 (!) through 126 (~) — printable minus space.
graphicChars :: [Char]
graphicChars = [chr 33 .. chr 126]

-- | Full ASCII range: 0 through 127.
asciiChars :: [Char]
asciiChars = [chr 0 .. chr 127]

-- | Control characters: 0 through 31.
controlChars :: [Char]
controlChars = [chr 0 .. chr 31]

-- | Uppercase letters.
uppercaseChars :: [Char]
uppercaseChars = ['A'..'Z']

-- | Lowercase letters.
lowercaseChars :: [Char]
lowercaseChars = ['a'..'z']

-- | Alphabetic characters: A-Z, a-z.
alphabeticChars :: [Char]
alphabeticChars = uppercaseChars ++ lowercaseChars

-- | Numeric digit characters: 0-9.
numericChars :: [Char]
numericChars = ['0'..'9']

-- | Alphanumeric characters: A-Z, a-z, 0-9.
alphanumericChars :: [Char]
alphanumericChars = alphabeticChars ++ numericChars

-- | Word characters string: [a-zA-Z0-9_] — used by \\w.
wordCharString :: String
wordCharString = alphanumericChars ++ "_"

-- | Non-word characters (matching Racket's exact list).
nonWordChars :: [Char]
nonWordChars = "!@#$%^&*()-+={}[]|\\:;\"'<>?,./`~ "

-- | Non-digit characters (matching Racket's exact hardcoded list).
nonDigitChars :: [Char]
nonDigitChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"

-- | Blank characters: space and tab.
blankChars :: [Char]
blankChars = [' ', '\t']

-- | Whitespace characters: space, tab, newline, carriage return.
whitespaceChars :: [Char]
whitespaceChars = [' ', '\t', '\n', '\r']

-- | Non-whitespace characters: printable chars minus whitespace.
nonWhitespaceChars :: [Char]
nonWhitespaceChars = filter (`notElem` whitespaceChars) printableChars

-- | Punctuation characters (matching Racket's exact hardcoded list).
punctuationChars :: [Char]
punctuationChars = "!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"

-- | Hex digit characters.
hexDigitChars :: [Char]
hexDigitChars = "0123456789abcdefABCDEF"

-- ---------------------------------------------------------------------------
-- Precomputed arrays for O(1) random access
-- ---------------------------------------------------------------------------

printableArr, wordArr, nonWordArr, whitespaceArr, nonWhitespaceArr :: CharArray
digitArr, nonDigitArr :: CharArray

printableArr      = charArrayFromList printableChars
wordArr           = charArrayFromList wordCharString
nonWordArr        = charArrayFromList nonWordChars
whitespaceArr     = charArrayFromList whitespaceChars
nonWhitespaceArr  = charArrayFromList nonWhitespaceChars
digitArr          = charArrayFromList numericChars
nonDigitArr       = charArrayFromList nonDigitChars

-- ---------------------------------------------------------------------------
-- Random character generators
-- ---------------------------------------------------------------------------

-- | Pick a random element from a CharArray.
arrayRandomRef :: Config -> CharArray -> IO Char
arrayRandomRef cfg arr = do
  let len = charArrayLength arr
  if len == 0
    then return '\0'
    else do
      idx <- randstrRandom cfg len
      return (charArrayIndex arr idx)

-- | Random printable character (ASCII 32-126).
randomCharacter :: Config -> IO Char
randomCharacter cfg = arrayRandomRef cfg printableArr

-- | Random word character: [a-zA-Z0-9_].
randomWordChar :: Config -> IO Char
randomWordChar cfg = arrayRandomRef cfg wordArr

-- | Random non-word character.
randomNonWordChar :: Config -> IO Char
randomNonWordChar cfg = arrayRandomRef cfg nonWordArr

-- | Random whitespace character.
randomWhitespaceChar :: Config -> IO Char
randomWhitespaceChar cfg = arrayRandomRef cfg whitespaceArr

-- | Random non-whitespace character.
randomNonWhitespaceChar :: Config -> IO Char
randomNonWhitespaceChar cfg = arrayRandomRef cfg nonWhitespaceArr

-- | Random digit character.
randomDigitChar :: Config -> IO Char
randomDigitChar cfg = arrayRandomRef cfg digitArr

-- | Random non-digit character.
randomNonDigitChar :: Config -> IO Char
randomNonDigitChar cfg = arrayRandomRef cfg nonDigitArr

-- ---------------------------------------------------------------------------
-- POSIX character class lookup
-- ---------------------------------------------------------------------------

-- | Lookup POSIX class name and return the character list.
posixClassChars :: String -> [Char]
posixClassChars name = case name of
  "alpha"  -> alphabeticChars
  "digit"  -> numericChars
  "alnum"  -> alphanumericChars
  "word"   -> wordCharString
  "blank"  -> blankChars
  "space"  -> whitespaceChars
  "upper"  -> uppercaseChars
  "lower"  -> lowercaseChars
  "ascii"  -> asciiChars
  "cntrl"  -> controlChars
  "graph"  -> graphicChars
  "print"  -> printableChars
  "punct"  -> punctuationChars
  "xdigit" -> hexDigitChars
  _        -> []  -- Unknown POSIX class

-- ---------------------------------------------------------------------------
-- Unicode property support
-- ---------------------------------------------------------------------------

-- | Cache for Unicode property ranges.
{-# NOINLINE unicodeRangeCache #-}
unicodeRangeCache :: IORef (Map.Map String [(Int, Int)])
unicodeRangeCache = unsafePerformIO $ newIORef Map.empty

-- | Normalize a Unicode property name to a canonical form.
normalizeUnicodePropertyName :: String -> String
normalizeUnicodePropertyName name
  | '=' `elem` name = name  -- Pass through Script=X, Block=X, etc.
  | otherwise = case name of
      -- General categories (long → short)
      "Letter"              -> "L"
      "Lowercase_Letter"    -> "Ll"
      "Uppercase_Letter"    -> "Lu"
      "Titlecase_Letter"    -> "Lt"
      "Modifier_Letter"     -> "Lm"
      "Other_Letter"        -> "Lo"
      "Mark"                -> "M"
      "Nonspacing_Mark"     -> "Mn"
      "Spacing_Mark"        -> "Mc"
      "Enclosing_Mark"      -> "Me"
      "Number"              -> "N"
      "Decimal_Number"      -> "Nd"
      "Letter_Number"       -> "Nl"
      "Other_Number"        -> "No"
      "Punctuation"         -> "P"
      "Connector_Punctuation" -> "Pc"
      "Dash_Punctuation"    -> "Pd"
      "Open_Punctuation"    -> "Ps"
      "Close_Punctuation"   -> "Pe"
      "Initial_Punctuation" -> "Pi"
      "Final_Punctuation"   -> "Pf"
      "Other_Punctuation"   -> "Po"
      "Symbol"              -> "S"
      "Math_Symbol"         -> "Sm"
      "Currency_Symbol"     -> "Sc"
      "Modifier_Symbol"     -> "Sk"
      "Other_Symbol"        -> "So"
      "Separator"           -> "Z"
      "Space_Separator"     -> "Zs"
      "Line_Separator"      -> "Zl"
      "Paragraph_Separator" -> "Zp"
      "Other"               -> "C"
      "Control"             -> "Cc"
      "Format"              -> "Cf"
      "Surrogate"           -> "Cs"
      "Private_Use"         -> "Co"
      "Unassigned"          -> "Cn"
      _                     -> name  -- Already short form or unknown

-- | Get Unicode property ranges for a property specifier.
-- Returns a list of (start, end) codepoint pairs (inclusive).
getUnicodePropertyRanges :: String -> [(Int, Int)]
getUnicodePropertyRanges prop = unsafePerformIO $ do
  cache <- readIORef unicodeRangeCache
  case Map.lookup normalized cache of
    Just ranges -> return ranges
    Nothing -> do
      let ranges = computeUnicodePropertyRanges normalized
      writeIORef unicodeRangeCache (Map.insert normalized ranges cache)
      return ranges
  where
    normalized = normalizeUnicodePropertyName prop

-- | Compute the total number of characters in Unicode property ranges.
unicodePropertyCharCount :: String -> Int
unicodePropertyCharCount prop =
  sum [end - start + 1 | (start, end) <- getUnicodePropertyRanges prop]

-- | Get the character at a given index within Unicode property ranges.
unicodePropertyCharAtIndex :: String -> Int -> Char
unicodePropertyCharAtIndex prop idx = go (getUnicodePropertyRanges prop) idx
  where
    go [] _ = chr 0xFFFD  -- Replacement character as fallback
    go ((start, end):rest) i
      | i < rangeSize = chr (start + i)
      | otherwise     = go rest (i - rangeSize)
      where rangeSize = end - start + 1

-- Internal: compute ranges for a normalized property name
computeUnicodePropertyRanges :: String -> [(Int, Int)]
computeUnicodePropertyRanges prop = case prop of
  -- General Categories
  "L"  -> filterRanges isLetterCat
  "Lu" -> filterRanges (== UppercaseLetter)
  "Ll" -> filterRanges (== LowercaseLetter)
  "Lt" -> filterRanges (== TitlecaseLetter)
  "Lm" -> filterRanges (== ModifierLetter)
  "Lo" -> filterRanges (== OtherLetter)
  "M"  -> filterRanges isMarkCat
  "Mn" -> filterRanges (== NonSpacingMark)
  "Mc" -> filterRanges (== SpacingCombiningMark)
  "Me" -> filterRanges (== EnclosingMark)
  "N"  -> filterRanges isNumberCat
  "Nd" -> filterRanges (== DecimalNumber)
  "Nl" -> filterRanges (== LetterNumber)
  "No" -> filterRanges (== OtherNumber)
  "P"  -> filterRanges isPunctuationCat
  "Pc" -> filterRanges (== ConnectorPunctuation)
  "Pd" -> filterRanges (== DashPunctuation)
  "Ps" -> filterRanges (== OpenPunctuation)
  "Pe" -> filterRanges (== ClosePunctuation)
  "Pi" -> filterRanges (== InitialQuote)
  "Pf" -> filterRanges (== FinalQuote)
  "Po" -> filterRanges (== OtherPunctuation)
  "S"  -> filterRanges isSymbolCat
  "Sm" -> filterRanges (== MathSymbol)
  "Sc" -> filterRanges (== CurrencySymbol)
  "Sk" -> filterRanges (== ModifierSymbol)
  "So" -> filterRanges (== OtherSymbol)
  "Z"  -> filterRanges isSeparatorCat
  "Zs" -> filterRanges (== Space)
  "Zl" -> filterRanges (== LineSeparator)
  "Zp" -> filterRanges (== ParagraphSeparator)
  "C"  -> filterRanges isOtherCat
  "Cc" -> filterRanges (== Control)
  "Cf" -> filterRanges (== Format)
  "Cs" -> filterRanges (== Surrogate)
  "Co" -> filterRanges (== PrivateUse)
  "Cn" -> filterRanges (== NotAssigned)

  -- Binary properties (approximations using known ranges)
  "Alphabetic"    -> filterRanges isLetterCat  -- Approximate
  "White_Space"   -> [(0x09, 0x0D), (0x20, 0x20), (0x85, 0x85), (0xA0, 0xA0),
                      (0x1680, 0x1680), (0x2000, 0x200A), (0x2028, 0x2029),
                      (0x202F, 0x202F), (0x205F, 0x205F), (0x3000, 0x3000)]
  "Cased"         -> filterRanges (\c -> c == UppercaseLetter || c == LowercaseLetter || c == TitlecaseLetter)
  "Dash"          -> [(0x2D, 0x2D), (0x058A, 0x058A), (0x05BE, 0x05BE),
                      (0x1400, 0x1400), (0x1806, 0x1806), (0x2010, 0x2015),
                      (0x2E17, 0x2E17), (0x2E1A, 0x2E1A), (0x301C, 0x301C),
                      (0x3030, 0x3030), (0x30A0, 0x30A0), (0xFE31, 0xFE32),
                      (0xFE58, 0xFE58), (0xFE63, 0xFE63), (0xFF0D, 0xFF0D)]
  "Hex_Digit"     -> [(0x30, 0x39), (0x41, 0x46), (0x61, 0x66),
                      (0xFF10, 0xFF19), (0xFF21, 0xFF26), (0xFF41, 0xFF46)]
  "ASCII"         -> [(0, 127)]
  "Any"           -> [(0, 0x10FFFF)]
  "Emoji"         -> emojiRanges

  -- Scripts (hardcoded ranges matching Racket implementation)
  _ | isScriptProp prop -> scriptRanges (getScriptName prop)
    | isBlockProp prop  -> blockRanges (getBlockName prop)
    | otherwise         -> []

  where
    isScriptProp s = take 7 s == "Script=" || s `elem` knownScriptShortNames
    isBlockProp s  = take 6 s == "Block="

    getScriptName s
      | take 7 s == "Script=" = drop 7 s
      | otherwise              = s

    getBlockName s
      | take 6 s == "Block=" = drop 6 s
      | otherwise             = s

    knownScriptShortNames = ["Han", "Latin", "Greek", "Cyrillic", "Hiragana",
                             "Katakana", "Arabic"]

-- Category predicates for filterRanges
isLetterCat :: GeneralCategory -> Bool
isLetterCat c = c `elem` [UppercaseLetter, LowercaseLetter, TitlecaseLetter,
                           ModifierLetter, OtherLetter]

isMarkCat :: GeneralCategory -> Bool
isMarkCat c = c `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]

isNumberCat :: GeneralCategory -> Bool
isNumberCat c = c `elem` [DecimalNumber, LetterNumber, OtherNumber]

isPunctuationCat :: GeneralCategory -> Bool
isPunctuationCat c = c `elem` [ConnectorPunctuation, DashPunctuation,
  OpenPunctuation, ClosePunctuation, InitialQuote, FinalQuote, OtherPunctuation]

isSymbolCat :: GeneralCategory -> Bool
isSymbolCat c = c `elem` [MathSymbol, CurrencySymbol, ModifierSymbol, OtherSymbol]

isSeparatorCat :: GeneralCategory -> Bool
isSeparatorCat c = c `elem` [Space, LineSeparator, ParagraphSeparator]

isOtherCat :: GeneralCategory -> Bool
isOtherCat c = c `elem` [Control, Format, Surrogate, PrivateUse, NotAssigned]

-- | Build ranges by scanning Unicode codepoints and checking category.
-- Scans the full Unicode range (matching Racket's behavior).
-- Results are cached, so this scan only happens once per property.
filterRanges :: (GeneralCategory -> Bool) -> [(Int, Int)]
filterRanges predicate = buildRanges $ filter (predicate . generalCategory . chr) codepoints
  where
    -- Full Unicode range excluding surrogates (matching Racket)
    codepoints = [0..0xD7FF] ++ [0xE000..0x10FFFF]

-- | Build contiguous ranges from a sorted list of codepoints.
buildRanges :: [Int] -> [(Int, Int)]
buildRanges [] = []
buildRanges (x:xs) = go x x xs
  where
    go start end [] = [(start, end)]
    go start end (y:ys)
      | y == end + 1 = go start y ys
      | otherwise    = (start, end) : go y y ys

-- ---------------------------------------------------------------------------
-- Script and block ranges (matching Racket's hardcoded ranges)
-- ---------------------------------------------------------------------------

scriptRanges :: String -> [(Int, Int)]
scriptRanges name = case name of
  "Han"       -> [(0x2E80, 0x2E99), (0x2E9B, 0x2EF3), (0x2F00, 0x2FD5),
                  (0x3005, 0x3005), (0x3007, 0x3007), (0x3021, 0x3029),
                  (0x3038, 0x303B), (0x3400, 0x4DBF), (0x4E00, 0x9FFF),
                  (0xF900, 0xFAFF), (0x20000, 0x2A6DF), (0x2A700, 0x2B739),
                  (0x2B740, 0x2B81D), (0x2B820, 0x2CEA1), (0x2CEB0, 0x2EBE0),
                  (0x30000, 0x3134A)]
  "Latin"     -> [(0x41, 0x5A), (0x61, 0x7A), (0xC0, 0xD6), (0xD8, 0xF6),
                  (0xF8, 0x2B8), (0x1E00, 0x1EFF), (0x2C60, 0x2C7F),
                  (0xA720, 0xA7FF), (0xAB30, 0xAB6F), (0xFB00, 0xFB06),
                  (0xFF21, 0xFF3A), (0xFF41, 0xFF5A)]
  "Greek"     -> [(0x370, 0x373), (0x375, 0x377), (0x37A, 0x37D),
                  (0x37F, 0x37F), (0x384, 0x384), (0x386, 0x386),
                  (0x388, 0x38A), (0x38C, 0x38C), (0x38E, 0x3A1),
                  (0x3A3, 0x3FF), (0x1D00, 0x1D2B), (0x1F00, 0x1FFF)]
  "Cyrillic"  -> [(0x400, 0x4FF), (0x500, 0x52F), (0x2DE0, 0x2DFF),
                  (0xA640, 0xA69F), (0xFE2E, 0xFE2F)]
  "Hiragana"  -> [(0x3041, 0x3096), (0x309D, 0x309F), (0x1B001, 0x1B11F),
                  (0x1F200, 0x1F200)]
  "Katakana"  -> [(0x30A1, 0x30FA), (0x30FD, 0x30FF), (0x31F0, 0x31FF),
                  (0x32D0, 0x32FE), (0x3300, 0x3357), (0xFF66, 0xFF9D),
                  (0x1B000, 0x1B000)]
  "Arabic"    -> [(0x600, 0x6FF), (0x750, 0x77F), (0x8A0, 0x8FF),
                  (0xFB50, 0xFDFF), (0xFE70, 0xFEFF), (0x10E60, 0x10E7E),
                  (0x1EE00, 0x1EEFF)]
  _           -> []

blockRanges :: String -> [(Int, Int)]
blockRanges name = case name of
  "Basic_Latin"             -> [(0x0000, 0x007F)]
  "Latin-1_Supplement"      -> [(0x0080, 0x00FF)]
  "Latin_Extended-A"        -> [(0x0100, 0x017F)]
  "Latin_Extended-B"        -> [(0x0180, 0x024F)]
  "CJK_Unified_Ideographs" -> [(0x4E00, 0x9FFF)]
  "Cyrillic"                -> [(0x0400, 0x04FF)]
  "Arabic"                  -> [(0x0600, 0x06FF)]
  "Hiragana"                -> [(0x3040, 0x309F)]
  "Katakana"                -> [(0x30A0, 0x30FF)]
  "Greek_and_Coptic"        -> [(0x0370, 0x03FF)]
  _                         -> []

-- | Emoji ranges (matching Racket's definition).
emojiRanges :: [(Int, Int)]
emojiRanges =
  [ (0x2600, 0x26FF)   -- Miscellaneous Symbols
  , (0x2700, 0x27BF)   -- Dingbats
  , (0x1F300, 0x1F5FF) -- Miscellaneous Symbols and Pictographs
  , (0x1F600, 0x1F64F) -- Emoticons
  , (0x1F680, 0x1F6FF) -- Transport and Map Symbols
  , (0x1F900, 0x1F9FF) -- Supplemental Symbols and Pictographs
  , (0x1FA00, 0x1FA6F) -- Chess Symbols
  , (0x1FA70, 0x1FAFF) -- Symbols and Pictographs Extended-A
  , (0x1F1E0, 0x1F1FF) -- Regional Indicator Symbols
  , (0xFE00, 0xFE0F)   -- Variation Selectors
  , (0x200D, 0x200D)   -- Zero Width Joiner
  , (0x1F3FB, 0x1F3FF) -- Skin tone modifiers
  ]

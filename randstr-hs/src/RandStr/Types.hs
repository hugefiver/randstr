-- | Core types for the randstr tokenizer and generator.
module RandStr.Types
  ( Token(..)
  , TokenType(..)
  , Quantifier(..)
  , TokenContent(..)
  , CharArray
  , charArrayFromList
  , charArrayLength
  , charArrayIndex
  ) where

import Data.Array (Array, listArray, (!), bounds)

-- | An immutable array of characters, used for character classes.
type CharArray = Array Int Char

-- | Build a CharArray from a list of characters.
charArrayFromList :: [Char] -> CharArray
charArrayFromList [] = listArray (0, -1) []  -- empty array
charArrayFromList cs = listArray (0, length cs - 1) cs

-- | Get the length of a CharArray.
charArrayLength :: CharArray -> Int
charArrayLength arr =
  let (lo, hi) = bounds arr
  in if hi < lo then 0 else hi - lo + 1

-- | Index into a CharArray.
charArrayIndex :: CharArray -> Int -> Char
charArrayIndex = (!)

-- | Quantifier for controlling repetition of token generation.
data Quantifier
  = QStar                          -- ^ @*@ — 0 to maxRepeat times
  | QPlus                          -- ^ @+@ — 1 to maxRepeat times
  | QOptional                      -- ^ @?@ — 0 or 1 time
  | QExact !Int                    -- ^ @{n}@ — exactly n times
  | QNormal !Int !Int              -- ^ @{n+}@ or @{n++}@ — normal distribution around n, with order
  | QNormalRange !Int !Int !Int    -- ^ @{n1+n2}@ — normal distribution in range [n1, n1+n2], with order
  deriving (Show, Eq)

-- | The type of a parsed token.
data TokenType
  = TLiteral               -- ^ A literal character
  | TCharClass              -- ^ A character class @[...]@
  | TWordChar               -- ^ @\\w@
  | TNonWordChar            -- ^ @\\W@
  | TWhitespaceChar         -- ^ @\\s@
  | TNonWhitespaceChar      -- ^ @\\S@
  | TDigitChar              -- ^ @\\d@
  | TNonDigitChar           -- ^ @\\D@
  | TAny                    -- ^ @.@ — any printable character
  | TGroup                  -- ^ @(...)@ — unnamed group
  | TNamedGroup             -- ^ @(?<name>...)@ — named group
  | TBackreference          -- ^ @\\k<name>@ — backreference to named group
  | TUnicodeProperty        -- ^ @\\p{...}@ — Unicode property
  deriving (Show, Eq)

-- | A parsed token from the pattern string.
data Token = Token
  { tokenType      :: !TokenType
  , tokenContent   :: !TokenContent
  , tokenQuantifier :: !(Maybe Quantifier)
  } deriving (Show, Eq)

-- | The content associated with a token, varying by token type.
data TokenContent
  = ContentChar !Char                     -- ^ Single character (literal, escape shorthand)
  | ContentChars !CharArray               -- ^ Character class members
  | ContentString !String                 -- ^ Group pattern or backreference name
  | ContentNamedGroup !String !String     -- ^ Named group: (name, pattern)
  | ContentUnicode !String                -- ^ Unicode property specifier
  | ContentNone                           -- ^ No content (for shorthand types)
  deriving (Show, Eq)

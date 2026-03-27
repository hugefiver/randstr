-- | Public API for randstr — random string generation from regex-like patterns.
--
-- This is a Haskell reimplementation of the randstr Racket library.
--
-- Usage:
--
-- @
-- import RandStr
--
-- main :: IO ()
-- main = do
--   s <- randstr defaultConfig "[a-z]{5}"
--   putStrLn s  -- e.g. "qwert"
--
--   ss <- randstrN defaultConfig "[0-9]{3}" 10
--   mapM_ putStrLn ss  -- 10 random 3-digit strings
-- @
module RandStr
  ( -- * Core API
    randstr
  , randstrN
  , parseAndGenerate
    -- * Configuration
  , Config(..)
  , defaultConfig
    -- * Re-exports
  , tokenizePattern
  ) where

import RandStr.Config (Config(..), defaultConfig)
import RandStr.Tokenizer (tokenizePattern)
import RandStr.Generator (generateFromTokens)

-- | Generate a single random string matching the given pattern.
randstr :: Config -> String -> IO String
randstr cfg pattern = parseAndGenerate cfg pattern

-- | Generate @n@ random strings matching the given pattern.
randstrN :: Config -> String -> Int -> IO [String]
randstrN cfg pattern n = sequence $ replicate n (randstr cfg pattern)

-- | Parse a pattern and generate a random string.
parseAndGenerate :: Config -> String -> IO String
parseAndGenerate cfg pattern =
  let tokens = tokenizePattern pattern
  in generateFromTokens cfg tokens

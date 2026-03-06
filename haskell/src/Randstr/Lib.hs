-- | Public API for the randstr library.
--
-- This module provides functions for generating random strings
-- from regex-like patterns.
module Randstr.Lib
  ( -- * Core API
    randstr
  , randstrN
  , randstrWith
  , randstrNWith
    -- * Pattern parsing
  , tokenizePattern
  , parseCharacterClass
  , parseQuantifier
  , parseGroup
  , parseUnicodeProperty
  , rangeToList
    -- * Configuration
  , Config(..)
  , defaultConfig
  ) where

import Randstr.Types ()
import Randstr.Config (Config(..), defaultConfig)
import Randstr.Tokenizer
import Randstr.Generator (generateFromTokens)
import System.Random (newStdGen)

-- | Generate a single random string from a pattern.
-- Uses a new random generator each time.
randstr :: String -> IO String
randstr pattern = do
  gen <- newStdGen
  let cfg = defaultConfig gen
      tokens = tokenizePattern pattern
      (result, _) = generateFromTokens tokens cfg
  return result

-- | Generate n random strings from a pattern.
-- Uses a new random generator, threading it through all generations.
randstrN :: String -> Int -> IO [String]
randstrN pattern n = do
  gen <- newStdGen
  let cfg = defaultConfig gen
  return $ generateN pattern n cfg

-- | Generate a single random string from a pattern with a given config.
randstrWith :: Config -> String -> String
randstrWith cfg pattern =
  let tokens = tokenizePattern pattern
      (result, _) = generateFromTokens tokens cfg
  in result

-- | Generate n random strings from a pattern with a given config.
randstrNWith :: Config -> String -> Int -> [String]
randstrNWith cfg pattern n = generateN pattern n cfg

-- | Generate n strings, threading the config through.
generateN :: String -> Int -> Config -> [String]
generateN _ 0 _ = []
generateN pattern n cfg =
  let tokens = tokenizePattern pattern
      (result, cfg') = generateFromTokens tokens cfg
  in result : generateN pattern (n - 1) cfg'

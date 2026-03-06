-- | Configuration for the randstr library.
module Randstr.Config
  ( Config(..)
  , defaultConfig
  , configRandom
  , configRandomReal
  ) where

import System.Random (StdGen, uniformR)

-- | Configuration for random string generation.
data Config = Config
  { cfgMaxRepeat    :: Int     -- ^ Maximum repetition for * and + quantifiers
  , cfgSecureRandom :: Bool    -- ^ Whether to use cryptographically secure random
  , cfgGen          :: StdGen  -- ^ Random number generator
  } deriving (Show)

-- | Default configuration.
defaultConfig :: StdGen -> Config
defaultConfig g = Config
  { cfgMaxRepeat    = 5
  , cfgSecureRandom = False
  , cfgGen          = g
  }

-- | Generate a random non-negative integer less than n.
-- Returns (randomValue, newConfig).
configRandom :: Int -> Config -> (Int, Config)
configRandom n cfg =
  let (val, newGen) = uniformR (0, n - 1) (cfgGen cfg)
  in (val, cfg { cfgGen = newGen })

-- | Generate a random floating point number in [0, 1).
-- Returns (randomValue, newConfig).
configRandomReal :: Config -> (Double, Config)
configRandomReal cfg =
  let (val, newGen) = uniformR (0.0 :: Double, 0.9999999999999998) (cfgGen cfg)
  in (val, cfg { cfgGen = newGen })

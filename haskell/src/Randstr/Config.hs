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
-- Guards against non-positive n by clamping to 1.
configRandom :: Int -> Config -> (Int, Config)
configRandom n cfg
  | cfgSecureRandom cfg =
      error "Secure random mode (cfgSecureRandom=True) is not implemented in this build; cryptographically secure random generation is unavailable."
  | otherwise =
      let safeN         = if n <= 0 then 1 else n
          (val, newGen) = uniformR (0, safeN - 1) (cfgGen cfg)
      in (val, cfg { cfgGen = newGen })

-- | Generate a random floating point number in [0, 1).
-- Returns (randomValue, newConfig).
-- The upper bound ensures we never reach exactly 1.0.
configRandomReal :: Config -> (Double, Config)
configRandomReal cfg
  | cfgSecureRandom cfg =
      error "Secure random mode (cfgSecureRandom=True) is not implemented in this build; cryptographically secure random generation is unavailable."
  | otherwise =
      let upperBound = 1.0 - 2.220446049250313e-16  -- 1.0 - machine epsilon for Double
          (val, newGen) = uniformR (0.0 :: Double, upperBound) (cfgGen cfg)
      in (val, cfg { cfgGen = newGen })

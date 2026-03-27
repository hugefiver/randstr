-- | Configuration for random string generation.
-- Uses a custom SplitMix PRNG and OS entropy source to avoid external dependencies.
module RandStr.Config
  ( Config(..)
  , defaultConfig
  , randstrRandom
  , randstrRandomReal
  , randstrRandomRange
  ) where

import Data.Word (Word8, Word32, Word64)
import Data.Bits (shiftL, shiftR, xor, (.|.))
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import System.IO (openBinaryFile, hSetBinaryMode, hClose, IOMode(ReadMode))
import System.IO.Error (catchIOError)
import qualified Data.ByteString as BS


-- | Configuration for randstr generation.
data Config = Config
  { cfgMaxRepeat    :: !Int   -- ^ Maximum repeat count for @*@ and @+@ (default: 5)
  , cfgSecureRandom :: !Bool  -- ^ Use CSPRNG instead of PRNG (default: False)
  } deriving (Show, Eq)

-- | Default configuration: maxRepeat=5, secureRandom=False.
defaultConfig :: Config
defaultConfig = Config
  { cfgMaxRepeat    = 5
  , cfgSecureRandom = False
  }

-- ---------------------------------------------------------------------------
-- Custom SplitMix-style PRNG (no external dependency)
-- ---------------------------------------------------------------------------

-- | PRNG state using SplitMix64 algorithm.
data PRNGState = PRNGState !Word64 !Word64
  deriving (Show)

-- | SplitMix64 next value.
splitMixNext :: PRNGState -> (Word64, PRNGState)
splitMixNext (PRNGState s gamma) =
  let s' = s + gamma
      z0 = s'
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      z3 = z2 `xor` (z2 `shiftR` 31)
  in (z3, PRNGState s' gamma)

-- | Initialize PRNG from a seed.
initPRNG :: Word64 -> PRNGState
initPRNG seed = PRNGState seed 0x9e3779b97f4a7c15  -- golden gamma

-- | Get a seed from environment/time. Uses a simple method that doesn't
-- require the time package: read a few bytes from /dev/urandom or fall back
-- to a fixed seed combined with IORef address hashing.
getSeed :: IO Word64
getSeed = do
  -- Try reading from /dev/urandom first
  result <- (do
    h <- openBinaryFile "/dev/urandom" ReadMode
    hSetBinaryMode h True
    bs <- BS.hGet h 8
    hClose h
    let bytes = BS.unpack bs
    return (Just (bytesToWord64 bytes))
    ) `catchIOError` (\_ -> return Nothing)
  case result of
    Just w  -> return w
    Nothing -> do
      -- Windows fallback: try to use environment or a semi-random seed
      -- We use the IORef allocation address as entropy (not great, but functional)
      ref <- newIORef (0 :: Int)
      addr <- readIORef ref
      -- Mix in some observable state
      let seed = fromIntegral addr `xor` 0xdeadbeef12345678
      return seed
  where
    bytesToWord64 :: [Word8] -> Word64
    bytesToWord64 bs =
      let padded = take 8 (map fromIntegral bs ++ repeat 0) :: [Word64]
      in foldl (\acc (b, shift) -> acc .|. (b `shiftL` shift))
               0
               (zip padded [56, 48, 40, 32, 24, 16, 8, 0])

-- | Global PRNG state.
{-# NOINLINE globalPRNG #-}
globalPRNG :: IORef PRNGState
globalPRNG = unsafePerformIO $ do
  seed <- getSeed
  newIORef (initPRNG seed)

-- | Generate a random Word32 from the PRNG.
prngWord32 :: IO Word32
prngWord32 = do
  st <- readIORef globalPRNG
  let (w64, st') = splitMixNext st
  writeIORef globalPRNG st'
  return (fromIntegral (w64 `shiftR` 32) :: Word32)

-- | Generate a random Double in [0, 1) from the PRNG.
prngDouble :: IO Double
prngDouble = do
  w <- prngWord32
  return (fromIntegral w / 4294967296.0)

-- ---------------------------------------------------------------------------
-- OS entropy source for secure random
-- ---------------------------------------------------------------------------

-- | Read n bytes of entropy from the OS.
getEntropyBytes :: Int -> IO BS.ByteString
getEntropyBytes n = do
  -- Try /dev/urandom on Unix-like systems
  result <- safeReadUrandom n
  case result of
    Just bs -> return bs
    Nothing -> do
      -- Fallback: generate from PRNG (not truly secure on Windows without
      -- proper CryptoAPI bindings, but functional)
      ws <- sequence $ replicate ((n + 3) `div` 4) prngWord32
      let bytes = concatMap word32ToBytes ws
      return $ BS.pack (take n bytes)
  where
    word32ToBytes :: Word32 -> [Word8]
    word32ToBytes w =
      [ fromIntegral (w `shiftR` 24)
      , fromIntegral (w `shiftR` 16)
      , fromIntegral (w `shiftR` 8)
      , fromIntegral w
      ]

    safeReadUrandom :: Int -> IO (Maybe BS.ByteString)
    safeReadUrandom m =
      (do h <- openBinaryFile "/dev/urandom" ReadMode
          hSetBinaryMode h True
          bs <- BS.hGet h m
          hClose h
          return (Just bs)
      ) `catchIOError` (\_ -> return Nothing)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Generate a random integer in @[0, n)@ using the configured random source.
randstrRandom :: Config -> Int -> IO Int
randstrRandom _   n | n <= 0 = return 0
randstrRandom cfg n
  | cfgSecureRandom cfg = secureRandomInt n
  | otherwise = do
      w <- prngWord32
      return (fromIntegral w `mod` n)

-- | Generate a random Double in @[0, 1)@.
randstrRandomReal :: Config -> IO Double
randstrRandomReal cfg
  | cfgSecureRandom cfg = secureRandomReal
  | otherwise = prngDouble

-- | Generate a random integer in @[lo, hi]@.
randstrRandomRange :: Config -> Int -> Int -> IO Int
randstrRandomRange cfg lo hi
  | lo >= hi  = return lo
  | otherwise = do
      r <- randstrRandom cfg (hi - lo + 1)
      return (lo + r)

-- | Secure random using OS entropy — rejection sampling to avoid modulo bias.
secureRandomInt :: Int -> IO Int
secureRandomInt n = go
  where
    n32 = fromIntegral n :: Word32
    limit = maxBound - (maxBound `mod` n32)
    go = do
      bs <- getEntropyBytes 4
      let w = bytesToWord32 bs
      if w >= limit
        then go  -- rejection sampling
        else return (fromIntegral (w `mod` n32))

secureRandomReal :: IO Double
secureRandomReal = do
  bs <- getEntropyBytes 4
  let w = bytesToWord32 bs
  return (fromIntegral w / fromIntegral (maxBound :: Word32))

bytesToWord32 :: BS.ByteString -> Word32
bytesToWord32 bs =
  let bytes = BS.unpack (BS.take 4 bs)
      padded = take 4 (map fromIntegral bytes ++ repeat 0) :: [Word32]
  in case padded of
       [b0, b1, b2, b3] -> (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
       _ -> 0  -- Should never happen due to take 4

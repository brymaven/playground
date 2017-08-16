{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Naive where

import Prelude hiding (concat)

import qualified Crypto.Hash as Cr
import Crypto.Hash.Algorithms (SHA256)
import Data.ByteArray (convert)
import Data.ByteString (ByteString, concat, empty)
import Data.ByteString.Char8 (pack)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

import Data.Foldable (maximumBy)
import Data.Ord (comparing)

data Block = Block
  { index :: Int
  , timestamp :: POSIXTime
  , hash :: ByteString
  , previousHash :: ByteString
  , body :: ByteString
  } deriving (Show)

sha256 :: ByteString -> Cr.Digest SHA256
sha256 = Cr.hash

sha256bs :: ByteString -> ByteString
sha256bs = convert . sha256

-- A block needs to be hashed to keep the integrity of the data.
hashBlock :: Block -> ByteString
hashBlock (Block i t _ p b) = concat [toByteString i, p, toByteString t, b]
  where
    toByteString :: Show a => a -> ByteString
    toByteString = pack . show

--
getLatestBlock :: [Block] -> Block
getLatestBlock = head

--
nextIndex :: Block -> Int
nextIndex = (+ 1) . index

generateNextBlock :: Block -> ByteString -> IO Block
generateNextBlock prevBlock blockData = do
  nextTimestamp <- getPOSIXTime
  let i = nextIndex prevBlock
  let block = Block i nextTimestamp empty (hash prevBlock) blockData
      nextHash = hashBlock block
  return $ block {hash = nextHash}

-- Genesis-block
getGenesisBlock :: IO Block
getGenesisBlock =
  getPOSIXTime >>= \t ->
    return $
    Block
      0
      t
      ""
      "816534932c2b7154836da6afc367695e6337db8a921823784c14378abed4f7d7"
      "my genesis block!!"

-- Validate previous bock
isValidBlock :: Block -> Block -> Bool
isValidBlock previous current = and [validIndex, validPrevHash, validHash]
 where validIndex = index previous + 1 == index current
       validPrevHash = previousHash current == hash previous
       validHash = hashBlock current == hash current

-- There should always be only one explicit set of blocks in the chain at a given time.
-- In case of conflict, we chooise the chain that has the longest number of blocks.
replaceChain :: [Block] -> [Block] -> [Block]
replaceChain chain newBlocks = maxBy length [chain, newBlocks]
  where
    maxBy = maximumBy . comparing
-- Proof-of-work
-- Scanning for a value that when hashed, such as with SHA-256, the hash begins with a number of zero bits.
-- Implementing a nonce in the block
-- Nonce is an arbitrary number that may only be used once
-- Incrementing a nonce in the block until a value is found
-- Once CPU effort has been expended, the block cannot be changed without redoing the work.
-- PoW is one-CPU-one-vote
-- If majority of CPU power is controlled by honest nodes, the honest chain will grow the fastest
-- 5. Network
-- New transacions are broadcast to all nodes
-- Transactions
-- e-coin as a chain of digital signatures. Sign pub key of the next owner to the end of the chain.
-- Bitcoin wallet contains one of more private keys


-- Check out Fae. Functional alternative to Ethereum

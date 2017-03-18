{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Api.Blockchain where

import           Data.Aeson
import           Data.Aeson.Types                   (emptyArray)

import qualified Data.HexString                         as HS
import qualified Data.Bitcoin.Block                     as Btc
import qualified Data.Bitcoin.Types                     as BT
import qualified Network.Bitcoin.Api.Internal           as I
import qualified Network.Bitcoin.Api.Types              as T
import qualified Network.Bitcoin.Api.Types.TxInfo       as TXI
import qualified Network.Bitcoin.Api.Types.HeaderInfo   as HDI
import qualified Data.Base58String                      as B58S

-- | Gets the amount of blocks currently in the blockchain, also known as the
--   'height' of the blockchain.
getBlockCount :: T.Client -> IO Integer
getBlockCount client =
  I.call client "getblockcount" emptyArray

-- | Get the hash of a block based on its offset (height).
getBlockHash :: T.Client        -- ^ Our client context
             -> Integer         -- ^ The height/offset of the block. 0 is the genesis block.
             -> IO BT.BlockHash -- ^ The hash of the block
getBlockHash client offset =
  let configuration = [toJSON offset]

  in I.call client "getblockhash"  configuration

-- | Gets a block based on its hash.
getBlock :: T.Client     -- ^ Our session context
         -> HS.HexString -- ^ Hexadecimal representation of the hash of a block
         -> IO (Maybe Btc.Block) -- ^ The block
getBlock client hash =
  let configuration = [toJSON hash, toJSON False]

  in fmap Btc.decode <$> I.callMaybe client "getblock" configuration

-- | Gets a block header based on its hash.
getBlockHeader :: T.Client          -- ^ Our session context
         -> HS.HexString            -- ^ Hexadecimal representation of the hash of a block
         -> IO (Maybe HS.HexString) -- ^ Hex-encoded block header
getBlockHeader client hash =
  let configuration = [toJSON hash, toJSON False]

  in I.callMaybe client "getblockheader" configuration

-- | Gets information about a block header based its hash.
getBlockHeaderInfo
         :: T.Client                    -- ^ Our session context
         -> HS.HexString                -- ^ Hexadecimal representation of the hash of a block
         -> IO (Maybe HDI.HeaderInfo)   -- ^ Block information
getBlockHeaderInfo client hash =
  let configuration = [toJSON hash, toJSON True]

  in I.callMaybe client "getblockheader" configuration

-- | NOTE: Only applicable for Bitcoin Core with the addrindex patch
--      Fetch information about transactions paying to or redeeming specified address
searchRawTransactions ::
    T.Client             -- ^ Our session context
    -> B58S.Base58String -- ^ Address of interest
    -> IO [TXI.TxInfo]   -- ^ List of TxInfo's either paying to or redeeming address
searchRawTransactions client addr =
  I.call client "searchrawtransactions" [toJSON addr]

-- | NOTE: Requires enabled transaction index in Bitcoin Core ("txindex" option)
--      Fetch hex-encoded transaction by transaction ID
getRawTransaction ::
    T.Client                    -- ^ Our session context
    -> BT.TransactionId         -- ^ Transaction ID of transaction to fetch
    -> IO (Maybe HS.HexString)  -- ^ Hex-encoded transaction
getRawTransaction client txid =
  I.callMaybe client "getrawtransaction" [toJSON txid, toJSON (0 :: Integer)]


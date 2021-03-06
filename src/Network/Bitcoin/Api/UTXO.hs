module Network.Bitcoin.Api.UTXO where

import           Data.Word           (Word32)
import           Data.Maybe          (maybeToList)
import           Data.Aeson

import qualified Data.Bitcoin.Types                     as BT
import qualified Network.Bitcoin.Api.Internal           as I
import qualified Network.Bitcoin.Api.Types              as T
import qualified Network.Bitcoin.Api.Types.UnspentTxOut as UTXO
import qualified Data.HexString                         as HS

type IncludeMempool = Bool

-- | Returns details about an unspent transaction output.
getTxOut :: T.Client                        -- ^ Our session context
         -> BT.TransactionId                -- ^ Transaction ID of transaction containing output
         -> Word32                          -- ^ Vout/index of output in transaction
         -> IncludeMempool                  -- ^ Return transaction outputs from unconfirmed transactions as well?
         -> IO (Maybe UTXO.UnspentTxOut)    -- ^ The unspent transaction output, if present in UTXO set
getTxOut client txid index includeMempool =
  let configuration = [toJSON txid, toJSON index, toJSON includeMempool]

  in I.call client "gettxout" configuration

-- | Returns proof that one or more transactions were included in a block
txOutProof :: T.Client                  -- ^ Our session context
           -> [BT.TransactionId]        -- ^ Transaction IDs to return proof for
           -> Maybe BT.BlockHash        -- ^ Optionally look in only this block
           -> IO (Maybe HS.HexString)   -- ^ Hex-encoded proof (see: https://bitcoin.org/en/developer-reference#merkleblock)
txOutProof client txidLst hashM =
  let configuration = toJSON txidLst : fmap toJSON (maybeToList hashM)

  in I.call client "gettxoutproof" configuration

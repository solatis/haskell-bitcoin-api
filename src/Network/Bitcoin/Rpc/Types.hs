module Network.Bitcoin.Rpc.Types where

import qualified Data.HexString as HS

-- | Per Bitcoin documentation, An identifier used to uniquely identify a
--   particular transaction; specifically, the sha256d hash of the transaction.
type TransactionId = HS.HexString

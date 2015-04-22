module Network.Bitcoin.Rpc.Types ( TransactionId
                                 , Address
                                 , Account
                                 , Btc ) where

import           Data.Fixed
import qualified Data.HexString as HS
import qualified Data.Text      as T

-- | Per Bitcoin documentation, An identifier used to uniquely identify a
--   particular transaction; specifically, the sha256d hash of the transaction.
type TransactionId = HS.HexString

-- | Per Bitcoin documentation, an identifier used to uniquely identify a
--   particular address.
type Address = T.Text

-- | A wallet account can be any easy to remember string.
type Account = T.Text

-- | The smallest unit of payment possible in Bitcoin is a Satoshi
data Satoshi = Satoshi

-- | We describe BTC in terms of Satoshi, where one BTC equals 10^8 Satoshis.
instance HasResolution Satoshi where
  resolution _ = 10 ^ ( 8 :: Integer )

-- | A single Bitcoin, which represents 10^8 Satoshis.
type Btc = Fixed Satoshi

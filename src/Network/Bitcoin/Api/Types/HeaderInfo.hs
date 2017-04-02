{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Network.Bitcoin.Api.Types.HeaderInfo where

import qualified Data.HexString                     as HS
import qualified Data.Bitcoin.Block                 as Btc
import qualified Data.Bitcoin.Types                 as BT
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Word              (Word32)
import           GHC.Generics           (Generic)


-- | Contains information about a block header, as returned by the
--   "getblockheader" RPC command, without the actual header.
data HeaderInfo = HeaderInfo
   { hash            :: BT.BlockHash
   , confirmations   :: Integer
   , height          :: Integer
   , mediantime      :: Word32
   , difficulty      :: Double
   , nextblockhash   :: Maybe BT.BlockHash
   , chainwork       :: BT.BlockHash
   } deriving
        (Eq, Show) -- , Generic, FromJSON, ToJSON)


instance FromJSON HeaderInfo where
  parseJSON = withObject "HeaderInfo" $ \o -> HeaderInfo
     <$> o .:  "hash"
     <*> o .:  "confirmations"
     <*> o .:  "height"
     <*> o .:  "mediantime"
     <*> o .:  "difficulty"
     <*> o .:? "nextblockhash"
     <*> o .:  "chainwork"


-- parseBlockHeader :: Value -> Parser Btc.BlockHeader
-- parseBlockHeader = withObject "BlockHeader" $ \o ->
--     Btc.BlockHeader
--         <$> o .: "version"
--         <*> o .: "previousblockhash"
--         <*> o .: "merkleroot"
--         <*> o .: "time"
--         <*> o .: "bits"     -- TODO: ByteString -> Word32 ("bits": "1803e6d4")
--         <*> o .: "nonce"

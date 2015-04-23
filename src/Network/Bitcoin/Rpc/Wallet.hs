{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Rpc.Wallet where

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.Text                                    as T

import qualified Data.HashMap.Strict as HM

import qualified Network.Bitcoin.Internal                     as I
import qualified Network.Bitcoin.Rpc.Types                    as RT
import           Network.Bitcoin.Rpc.Types.UnspentTransaction (UnspentTransaction)
import qualified Network.Bitcoin.Types                        as T

-- | Lists unspent transaction with default parameters
listUnspent :: T.Client
            -> IO [UnspentTransaction]
listUnspent client = listUnspentWith client 1 9999999

-- | Lists unspent transactions with configurable parameters
listUnspentWith :: T.Client -- ^ Our client context
                -> Integer  -- ^ Minimum amount of confirmations needed. Defaults to 1.
                -> Integer  -- ^ Maximum amount of confirmations. Defaults to 9999999.
                -> IO [UnspentTransaction]
listUnspentWith client confMin confMax =
  let configuration = [toJSON confMin, toJSON confMax, emptyArray]

  in I.call client "listunspent" configuration

-- | Lists all accounts currently known by the wallet with default parameters
listAccounts :: T.Client
             -> IO [(RT.Account, RT.Btc)]
listAccounts client = listAccountsWith client 1 False

-- | Lists all accounts currently known by the wallet with configurable parameters
listAccountsWith :: T.Client -- ^ Our client context
                 -> Integer  -- ^ Minimum amount of confirmations a transaction needs
                 -> Bool     -- ^ Whether or not to include watch-only addresses
                 -> IO [(RT.Account, RT.Btc)]
listAccountsWith client confirmations watchOnly =
  let configuration = [toJSON confirmations, toJSON watchOnly]

  in
    return . HM.toList =<< I.call client "listaccounts" configuration

-- | Provides access to a new receiving address filed under the default account.
--   Intended to be published to another party that wishes to send you money.
newAddress :: T.Client         -- ^ Our client context
           -> IO RT.Address    -- ^ The address created
newAddress client = newAddressWith client (T.pack "")

-- | Provides access to a new receiving address filed under a specific account.
--   Intended to be published to another party that wishes to send you money.
newAddressWith :: T.Client      -- ^ Our client context
               -> RT.Account    -- ^ The account to create the address under
               -> IO RT.Address -- ^ The address created
newAddressWith client account =
  let configuration = [account]

  in I.call client "getnewaddress" configuration

-- | Provides access to a new change address, which will not appear in the UI.
--   This is to be used with raw transactions only.
newChangeAddress :: T.Client         -- ^ Our client context
                 -> IO RT.Address    -- ^ The address created
newChangeAddress client =
  I.call client "getrawchangeaddress" emptyArray

-- | Provides access to the 'RT.Account' an 'RT.Address' belongs to.
getAddressAccount :: T.Client
                  -> RT.Address
                  -> IO RT.Account
getAddressAccount client address =
  let configuration = [address]
  in I.call client "getaccount" configuration

{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Api.Wallet where

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.HashMap.Strict                          as HM

import qualified Data.Bitcoin.Types                           as BT
import qualified Network.Bitcoin.Api.Internal                 as I
import qualified Network.Bitcoin.Api.Types                    as T
import           Network.Bitcoin.Api.Types.UnspentTransaction (UnspentTransaction)

-- | Lists unspent transaction with default parameters
listUnspent :: T.Client
            -> IO [UnspentTransaction]
listUnspent client = listUnspentWith client 1 9999999 []

-- | Lists unspent transactions with configurable parameters
listUnspentWith :: T.Client     -- ^ Our client context
                -> Integer      -- ^ Minimum amount of confirmations needed. Defaults to 1.
                -> Integer      -- ^ Maximum amount of confirmations. Defaults to 9999999.
                -> [BT.Address] -- ^ Return only results relevant to these addresses
                -> IO [UnspentTransaction]
listUnspentWith client confMin confMax addrList =
  let configuration = [toJSON confMin, toJSON confMax, toJSON addrList]

  in I.call client "listunspent" configuration

-- | Lists all accounts currently known by the wallet with default parameters
listAccounts :: T.Client
             -> IO [(BT.Account, BT.Btc)]
listAccounts client = listAccountsWith client 1 False

-- | Lists all accounts currently known by the wallet with configurable parameters
listAccountsWith :: T.Client -- ^ Our client context
                 -> Integer  -- ^ Minimum amount of confirmations a transaction needs
                 -> Bool     -- ^ Whether or not to include watch-only addresses
                 -> IO [(BT.Account, BT.Btc)]
listAccountsWith client confirmations watchOnly =
  let configuration        = [toJSON confirmations, toJSON watchOnly]

  in
    return . HM.toList =<< I.call client "listaccounts" configuration

-- | Returns the amount of Btc currently held in the wallet by a specified
--   account.
getAccountBalance :: T.Client         -- ^ Our client context
                  -> BT.Account       -- ^ The account we're looking for
                  -> IO BT.Btc        -- ^ Amount of Btc in wallet
getAccountBalance client accountId =
  -- FIXME: we should use the native 'getbalance' function here
  return . snd . head . filter ((== accountId) . fst) =<< listAccounts client

-- | Provides access to a new receiving address filed under the default account.
--   Intended to be published to another party that wishes to send you money.
newAddress :: T.Client         -- ^ Our client context
           -> IO BT.Address    -- ^ The address created
newAddress client =
  I.call client "getnewaddress" emptyArray

-- | Provides access to a new receiving address filed under a specific account.
--   Intended to be published to another party that wishes to send you money.
newAddressWith :: T.Client      -- ^ Our client context
               -> BT.Account    -- ^ The account to create the address under
               -> IO BT.Address -- ^ The address created
newAddressWith client account =
  let configuration = [account]
  in I.call client "getnewaddress" configuration

-- | Provides access to a new change address, which will not appear in the UI.
--   This is to be used with raw transactions only.
newChangeAddress :: T.Client         -- ^ Our client context
                 -> IO BT.Address    -- ^ The address created
newChangeAddress client =
  I.call client "getrawchangeaddress" emptyArray

-- | Provides access to the 'BT.Account' an 'BT.Address' belongs to.
getAddressAccount :: T.Client
                  -> BT.Address
                  -> IO BT.Account
getAddressAccount client address =
  let configuration = [address]
  in I.call client "getaccount" configuration

-- | Creates an off-blockchain transaction that moves bitcoin within a wallet
--   from one account to another.
move :: T.Client   -- ^ Our client context
     -> BT.Account -- ^ Account moving btc from
     -> BT.Account -- ^ Account moving btc to
     -> BT.Btc     -- ^ Amount of btc to move
     -> IO Bool    -- ^ Returns True if move was succesful
move client from to btc =
  let configuration = [toJSON from, toJSON to, toJSON btc]
  in I.call client "move" configuration


-- | Imports address into wallet, enabling showing the balance using 'listUnspent'
importAddress   :: T.Client   -- ^ Our client context
                -> BT.Address -- ^ Address to import
                -> BT.Account -- ^ Account with which the address will be associated
                -> Bool       -- ^ Rescan blockchain? (will take a while) default=True
                -> IO Value   -- ^ Returns nothing if everything went alright (as far as I can tell)
importAddress client address label rescan =
    let configuration = [toJSON address, toJSON label, toJSON rescan]
    in I.call client "importaddress" configuration

-- | Send amount to specified address, returning transaction ID of resulting Tx
sendToAddress :: T.Client
              -> BT.Address
              -> BT.Btc
              -> IO BT.TransactionId
sendToAddress client address amount =
    let configuration = [toJSON address, toJSON amount]
    in I.call client "sendtoaddress" configuration

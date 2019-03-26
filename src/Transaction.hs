module Transaction where

import Codec.Crypto.RSA( generateKeyPair
                       , encrypt, decrypt
                       , PrivateKey, PublicKey
                       , sign, verify
                       )
import Data.Hashable.Generic
import Data.ByteString.Lazy
import Data.Maybe
import Data.ByteString.Lazy.Char8 as BSC(pack)
import Control.Lens

import Types

makeTransaction :: (PrivateKey, PublicKey) -> PublicKey -> Integer -> Transaction
makeTransaction (sk, pk) to howmuch =
  let dat = TransactionData
        { _source = Just pk
        , _target = to
        , _amount = howmuch
        }

      hd = TransactionHeader
        { _signature = sign sk $ BSC.pack $ show dat
        }
  in Transaction
     { _transactionData = dat
     , _transactionHeader = hd
     }

makeReward :: (PrivateKey, PublicKey) -> Integer -> Transaction
makeReward (sk, pk) howmuch =
  let dat = TransactionData
        { _source = Nothing
        , _target = pk
        , _amount = howmuch
        }
  in Transaction
     { _transactionData = dat
     , _transactionHeader = TransactionHeader
       { _signature = sign sk $ BSC.pack $ show dat
       }
     }

transactionValid :: Transaction -> Bool
transactionValid t = case _source . _transactionData $ t of
  Nothing -> verify (_target . _transactionData $ t)
                      (BSC.pack . show . _transactionData $ t)
                      (_signature . _transactionHeader $ t)
  Just pk -> verify pk
                     (BSC.pack . show . _transactionData $ t)
                     (_signature . _transactionHeader $ t)

isReward :: Transaction -> Bool
isReward = not . isJust . _source . _transactionData

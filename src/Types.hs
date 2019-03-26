{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Types where

import Data.List.NonEmpty
import GHC.Generics
import Data.ByteString.Lazy
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Actor
import Codec.Crypto.RSA
import Data.Hashable.Generic
import Data.Map

deriving instance Generic PublicKey
instance Hashable PublicKey

type Hash = Int

data TransactionHeader = TransactionHeader
  { _signature :: ByteString
  } deriving (Eq, Show, Generic)

data TransactionData = TransactionData
  { _source :: Maybe PublicKey
  , _target :: PublicKey
  , _amount :: Integer
  } deriving (Eq, Generic)
instance Show TransactionData where
  show t = (Prelude.take 16 (show $ public_n <$> _source t))
           ++ " -> " ++ show (_amount t) ++ "$ -> "
           ++ (Prelude.take 16 (show $ public_n $ _target t))

data Transaction = Transaction
  { _transactionHeader :: TransactionHeader
  , _transactionData :: TransactionData
  } deriving (Eq, Generic)
instance Show Transaction where
  show = show . _transactionData

instance Hashable TransactionData
instance Hashable TransactionHeader
instance Hashable Transaction

data BlockHeader = BlockHeader
  { _previousHash :: Hash
  -- , _difficulty :: Int
  , _nonce :: Int
  } deriving (Eq, Show, Generic)
instance Hashable BlockHeader

data Block
  = Genesis {_transactions :: [Transaction]}
  | Block
    { _blockHeader :: BlockHeader
    , _transactions :: [Transaction]
    } deriving (Eq, Show, Generic)
instance Hashable Block


newtype Blockchain = Blockchain {blocks :: NonEmpty Block }
  deriving (Eq, Show)

type TransactionPool = [Transaction]

data ServerState = ServerState
  { _transactionPool :: TransactionPool
  , _blockchain :: Blockchain
  } deriving (Eq, Show)

data ClientQuery
  = ThisIsState ServerState
  | Rejected
  | Accepted
  | ClientStop

data ServerQuery
  = GiveMeState (Actor ClientQuery)
  | PushBlock (Actor ClientQuery) Block
  | PushTransaction Transaction
  | ThrowAPaperBall
  | ServerStop

makeLenses ''BlockHeader
makeLenses ''Block
makeLenses ''Transaction
makeLenses ''TransactionHeader
makeLenses ''TransactionData
makeLenses ''ServerState


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Types where

import Data.List.NonEmpty
import GHC.Generics
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Actor
import Codec.Crypto.RSA
import Data.Hashable.Generic

deriving instance Generic PublicKey
instance Hashable PublicKey

type Hash = Int

data Transaction = Transaction
  { _source :: Maybe PublicKey
  , _target :: PublicKey
  , _amount :: Integer
  } deriving (Eq, Show, Generic)

instance Hashable Transaction

data BlockHeader = BlockHeader
  { _previousHash :: Hash
  , _difficulty :: Int
  , _nonce :: Int
  } deriving (Eq, Show, Generic)
instance Hashable BlockHeader

data Block = Genesis | Block
  { _blockHeader :: BlockHeader
  , _transactions :: [Transaction]
  } deriving (Eq, Show, Generic)
instance Hashable Block


newtype Blockchain = Blockchain (NonEmpty Block)
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

data ServerQuery
  = GiveMeState (Actor ClientQuery)
  | PushBlock (Actor ClientQuery) Block
  | PushTransaction (Actor ClientQuery) Transaction
  | ThrowAPaperBall

makeLenses ''BlockHeader
makeLenses ''Block
makeLenses ''Transaction
makeLenses ''ServerState


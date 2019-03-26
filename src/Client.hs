{-# LANGUAGE ScopedTypeVariables #-}
module Client where

import qualified Data.List.NonEmpty as DLNE
import Control.Monad
import Data.IORef
import Control.Concurrent.Actor hiding (say)
import qualified Control.Concurrent.Actor as ACT (say)
import Codec.Crypto.RSA
import Control.Lens
import Crypto.Random

import Types
import Transaction
import Blockchain

say _ = pure ()
-- say = ACT.say

newKeyPair :: IO (PrivateKey, PublicKey)
newKeyPair = do
  (g :: SystemRandom) <- newGenIO -- this is bad habit
  let (p, s, _) = generateKeyPair g 2048
  pure (s, p)


runMiner :: (PrivateKey, PublicKey) -> Actor ServerQuery -> IO (Actor ClientQuery)
runMiner keys server = spawn $ minerActor keys server


minerActor :: (PrivateKey, PublicKey) -> Actor ServerQuery -> MBox ClientQuery -> IO ()
minerActor keys server mb = do
  me <- flip Actor mb <$> self
  say $ "Miner is running. Asking for state"
  void $ server ! GiveMeState me
  receive mb $ \case
    ClientStop -> say "Bye!" >> kill self  -- dramatic
    Rejected -> void $ server ! GiveMeState me
    Accepted -> void $ server ! GiveMeState me
    ThisIsState s -> do
      say $ "Recieved new state! " ++ show s
      let rewardTransaction = makeReward keys blockreward
          b = mineBlock $ over transactionPool (rewardTransaction:) s
          msg = "Mined! transactions: " ++ show (_transactions b)
      seq (length msg) $ say msg
      void $ server ! PushBlock me b


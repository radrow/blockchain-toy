module Client where

import qualified Data.List.NonEmpty as DLNE
import Control.Monad
import Data.IORef
import Control.Concurrent.Actor

import Types
import Blockchain

runMiner :: Actor ServerQuery -> IO (Actor ClientQuery)
runMiner server = do
  spawn $ minerActor server Nothing


minerActor :: Actor ServerQuery -> Maybe (IORef ServerState) -> MBox ClientQuery -> IO ()
minerActor server Nothing mb = do
  me <- flip Actor mb <$> self
  say $ "Miner is running. Asking for state"
  void $ server ! GiveMeState me
minerActor server (Just stateRef) mb = do
  me <- flip Actor mb <$> self
  state <- readIORef stateRef
  receive mb $ \case
    ClientStop -> say "Bye!" >> kill self  -- dramatic
    Rejected -> void $ server ! GiveMeState me
    Accepted -> void $ server ! GiveMeState me
    ThisIsState s -> do
      say $ "Recieved new state! transactions: " ++
        show (_transactions . DLNE.head . blocks . _blockchain $ s)
      writeIORef stateRef s
      let b = mineBlock s
      say $ "Mined! transactions: " ++ show (_transactions b)
      void $ server ! PushBlock me b

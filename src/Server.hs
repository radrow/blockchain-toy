module Server where

import Data.IORef
import qualified Data.List.NonEmpty as DLNE
import Control.Concurrent.Actor
import Control.Monad
import Control.Lens
import Data.Bifunctor
import qualified Data.Map as M
import Codec.Crypto.RSA

import Types
import Blockchain
import Transaction

runServer :: IO (Actor ServerQuery)
runServer = do
  initState <- newIORef $ ServerState [] (Blockchain (Genesis [] DLNE.:| []))
  say $ "Waiting for queries"
  spawn (serverActor initState)


serverActor :: IORef ServerState -> MBox ServerQuery -> IO ()
serverActor state mb = receive mb $ \case
  ServerStop -> say "Bye!" >> kill self
  GiveMeState who -> do
    say $ show who ++ " has asked for state"
    s <- readIORef state
    void $ who ! (ThisIsState s)
  ThrowAPaperBall -> say "Hey! Who did this?!"
  PushBlock who block -> do
    s <- readIORef state
    case insertBlock block s of
      Left errs -> do
        say $ "Invalid block. Reasons: " ++ unlines errs
        void $ who ! Rejected
      Right newState -> do
        writeIORef state newState
        say $ "Added new block! Wallets: " ++
          show (fmap (first (take 16 . show . public_n)) . M.toList . getWallet . _blockchain $ newState)
        void $ who ! Accepted
  PushTransaction tr ->
    if transactionValid tr
    then do
      say $ "Got valid transaction: " ++ show tr
      readIORef state >>= writeIORef state . pushTransaction tr
    else say "Invalid transaction received"

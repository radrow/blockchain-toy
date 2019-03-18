module Server where

import Data.IORef
import qualified Data.List.NonEmpty as DLNE
import Control.Concurrent.Actor
import Control.Monad
import Control.Lens

import Types
import Blockchain

runServer :: IO (Actor ServerQuery)
runServer = do
  initState <- newIORef $ ServerState [] (Blockchain (Genesis DLNE.:| []))
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
      Nothing -> void $ who ! Rejected
      Just newState -> do
        who ! Accepted
        writeIORef state newState
        say $ "Added new block! transactions: " ++
          show (_transactions . DLNE.head . blocks . _blockchain $ newState)
  PushTransaction who tr -> readIORef state >>= writeIORef state . pushTransaction tr

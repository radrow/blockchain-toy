module Server where

import Data.IORef
import Data.List.NonEmpty
import Control.Concurrent.Actor
import Control.Monad

import Types

runServer :: IO (Actor ServerQuery)
runServer = do
  initState <- newIORef $ ServerState [] (Blockchain (Genesis :| []))
  spawn (serverActor initState)


serverActor :: IORef ServerState -> MBox ServerQuery -> IO ()
serverActor state mb = receive mb $ \case
  GiveMeState who -> readIORef state >>= \s -> void $ who ! (ThisIsState s)
  ThrowAPaperBall -> putStrLn "Hey! Who did this?!"
  PushBlock who block -> do
    s <- readIORef state
    case insertBlock block s of
      Nothing -> void $ who ! Rejected
      Just newState -> who ! Accepted >> writeIORef newState s
  PushTransaction who tr -> pushTransaction tr

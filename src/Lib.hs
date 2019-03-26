module Lib
    ( someFunc
    , module Server
    , module Client
    , module Types
    , module Transaction
    ) where

import Control.Concurrent.Actor

import Server
import Client
import Types
import Transaction

someFunc :: IO ()
someFunc = putStrLn "someFunc"

test = do
  akp@(as, ap) <- newKeyPair
  bkp@(bs, bp) <- newKeyPair
  let tran = makeTransaction akp bp 30

  s <- runServer
  m <- runMiner akp s

  s ! PushTransaction tran
  pure tran

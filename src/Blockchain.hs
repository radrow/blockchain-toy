module Blockchain where

import qualified Data.List.NonEmpty as DLNE
import Data.Hashable.Generic

import Types


powCheck :: Block -> Bool
powCheck = (==0) . (`mod` 2) . hash


insertBlock :: Block -> ServerState -> Maybe ServerState
insertBlock b s =
  let trs = _transactions b
      hd = _blockHeader b
      (Blockchain blocks) = _blockchain s

      conditions =
        [ all (`elem` _transactionPool s) trs
        , _previousHash hd == hash (DLNE.head blocks)
        , powCheck b
        ]
  in if and conditions
  then Just $ ServerState
       { _blockchain = Blockchain $ DLNE.cons b blocks
       , _transactionPool = filter (`notElem` trs) $ _transactionPool s
       }
  else Nothing

module Blockchain where

import qualified Data.List.NonEmpty as DLNE
import Data.Hashable.Generic
import Control.Lens

import Types


powCheck :: Block -> Bool
powCheck = (==0) . (`mod` 2000000) . hash


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


pushTransaction :: Transaction -> ServerState -> ServerState
pushTransaction = over transactionPool . (:)


mineBlock :: ServerState -> Block
mineBlock s =
  let (Blockchain blocks) = _blockchain s
      initBlock = Block
        { _blockHeader = BlockHeader
          { _previousHash = hash $ DLNE.head blocks
          -- , difficulty = 0
          , _nonce = 0
          }
        , _transactions = _transactionPool s
        }

      mine :: Block -> Block
      mine b =
        if powCheck b
        then b else mine $ over (blockHeader . nonce) (+1) b
  in mine initBlock

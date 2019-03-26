{-# LANGUAGE StandaloneDeriving #-}
module Blockchain where

import qualified Data.List.NonEmpty as DLNE
import Data.Hashable.Generic
import Control.Lens
import Data.Map as M hiding (filter)
import Codec.Crypto.RSA
import Data.Bifunctor

import Types
import Transaction


blockreward :: Integer
blockreward = 100


powCheck :: Block -> Bool
powCheck = (==0) . (`mod` 2000000) . hash


insertBlock :: Block -> ServerState -> Either [String] ServerState
insertBlock b s =
  let trs = _transactions b
      hd = _blockHeader b
      (Blockchain blocks) = _blockchain s
      newchain = Blockchain $ DLNE.cons b blocks

      conditions =
        [ (all transactionValid trs, "valid transactions")
        , ( sum (fmap (_amount . _transactionData) $ filter isReward trs) <= blockreward
          , "block reward not exceeded"
          )
        , (_previousHash hd == hash (DLNE.head blocks), "prev hash match")
        , (all (>=0) (M.elems $ getWallet newchain), "no negative budgets")
        , (powCheck b, "PoW check")
        ]
  in if all fst conditions
  then Right $ ServerState
       { _blockchain = newchain
       , _transactionPool = filter (`notElem` trs) $ _transactionPool s
       }
  else Left $ fmap snd $ filter (not . fst) conditions


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

deriving instance Ord PublicKey

getWallet :: Blockchain -> Map PublicKey Integer
getWallet =
  go M.empty . (>>= fmap _transactionData . _transactions) . DLNE.toList . blocks where
    go acc [] = acc
    go acc (t:rest) =
      let withBenefit :: Map PublicKey Integer
          withBenefit = M.insert
                        (_target t)
                        (maybe (_amount t) (+(_amount t)) $ M.lookup (_target t) acc) acc
      in case _source t of
        Nothing -> go withBenefit rest
        Just s ->
          let withLoss :: Map PublicKey Integer
              withLoss = M.insert s
                (maybe (_amount t) (\x -> x - (_amount t)) $ M.lookup s withBenefit) withBenefit
          in go withLoss rest

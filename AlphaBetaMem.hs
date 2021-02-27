module AlphaBetaMem where

import Squava
import Data.List
import HashTreeDict

-- Memory is HashTreeDict to map States to best Action and value in given state
type Mem = Dict State (Action, Int)

-- alphabeta game st depth a b mem, where depth is length of gametree to search,
-- a is alpha, b is beta, mem is current transposition table
alphabeta_mem :: Game -> State -> Int -> Int -> Int -> Mem -> ((Action, Int), Mem)
alphabeta_mem game st depth a b mem =
   case getval st mem of
      Just act_val  -> (act_val,mem)
      Nothing ->
        let (act_val,alpha,mem1) = -- Use mem value if has calculated before
              abm_argmax (valueact game st depth) a b (sortOn (\x -> state_value (game x st)) avail) mem
        in (act_val, (insertval st act_val mem1)) -- adds to mem otherwise
    where State _ avail = st

-- finds move using minimax with alpha-beta pruning and transposition table
abm_player :: Int -> Game -> Player
abm_player depth game state = fst (fst (alphabeta_mem game state depth (-1000) 1000 emptyDict))

-- keeps track of largest value, alpha, and transposition table
abm_argmax :: (Int -> Int -> Mem -> Action -> (Int, Mem)) -> Int -> Int -> [Action] -> Mem -> ((Action,Int),Int,Mem)
abm_argmax f a b [e] mem = ((e, fe), (max a fe),mem1)
    where
        (fe, mem1) = f a b mem e
abm_argmax f a b (h:t) mem
   | at == 1000 || at >= b = ((bt, ft), 1000, mem2)
   | fh > ft                = ((h, fh), (max fh at), mem2)
   | otherwise              = ((bt, ft), (max ft at), mem2)
   where
        ((bt,ft),at,mem1) = abm_argmax f a b t mem
        (fh, mem2) = f at b mem1 h

valueact :: Game -> State -> Int -> Int -> Int -> Mem -> Action -> (Int, Mem)
valueact game st depth a b mem act = value game (game act st) depth a b mem

value :: Game -> Result -> Int -> Int -> Int -> Mem -> (Int, Mem)
value _ (EndOfGame _ val) _ _ _ mem = (val, mem)
value _ s 0 _ _ mem = (state_value s, mem)
value game (ContinueGame st) depth a b mem =
    let ((_,val), mem2) = alphabeta_mem game st (depth-1) (-b) (-a) mem
        in  (-val,mem2)
        
{- 
    abm_player 2 squava_ord squava_start
-}
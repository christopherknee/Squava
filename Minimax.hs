module Minimax where

-- Borrowed and built upon the CPSC 312 Minimax.hs

import Squava

-- minimax game st depth a b, where depth is length of gametree to search
minimax :: Game -> State -> Int -> (Action, Int)
minimax game st depth =
      argmax (valueact game st depth) avail
      where State _ avail = st

mm_player :: Int -> Game -> Player
mm_player depth game state = fst (minimax game state depth)

argmax :: Ord v => (e -> v) -> [e] -> (e,v)
argmax f [e] = (e, f e)
argmax f (h:t) 
   | fh > ft = (h,fh)
   | otherwise = (bt, ft)
   where
      (bt,ft) = argmax f t
      fh = f h

valueact :: Game -> State -> Int -> Action -> Int
valueact game st depth act = value game (game act st) depth

value :: Game -> Result -> Int -> Int
value _ (EndOfGame _ val) depth = val
value _ s 0 = state_value s
value game (ContinueGame st) depth =  - snd (minimax game st (depth - 1))

{- 
    mm_player 2 squava squava_start
-}
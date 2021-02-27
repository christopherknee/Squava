module AlphaBeta where

import Squava
import Data.List

-- alphabeta game st depth a b, where depth is length of gametree to search,
-- a is alpha, b is beta
alphabeta :: Game -> State -> Int -> Int -> Int -> (Action, Int)
alphabeta game st depth a b = 
    ab_argmax (valueact game st depth) a b ((Action 25),-1001) (sortOn (\x -> -(state_value (game x st))) avail)
        where State _ avail = st

-- finds move using minimax with alpha-beta pruning
ab_player :: Int -> Game -> Player
ab_player depth game state = fst (alphabeta game state depth (-1000) 1000)

-- keeps track of largest value and alpha
ab_argmax :: (Int -> Int -> Action -> Int) -> Int -> Int -> (Action,Int) -> [Action] -> (Action,Int)
ab_argmax _ _ _ p [] = p
ab_argmax f a b (m,fm) (h:t)
    | ares >= b = (h,fh)
    | fh > fm   = ab_argmax f ares b (h,fh) t
    | otherwise = ab_argmax f ares b (m,fm) t
    where
        fh = f a b h
        ares = max a fh

valueact :: Game -> State -> Int -> Int -> Int -> Action -> Int
valueact game st depth a b act = value game (game act st) depth a b

value :: Game -> Result -> Int -> Int -> Int -> Int
value _ (EndOfGame _ val) _ _ _ = val
value _ s 0 _ _ = state_value s
value game (ContinueGame st) depth a b =
    - snd (alphabeta game st (depth - 1) (-b) (-a))

{- 
    ab_player 4 squava squava_start
-}
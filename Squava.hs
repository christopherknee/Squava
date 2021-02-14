module Squava where


data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double          -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

-- Squava

data Action = Action Int                 -- a move for a player is just an Int
         deriving (Ord,Eq)
type InternalState = ([Action],[Action])   -- (self,other) actions done by P1 and P2

instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]

squava_start = State ([],[]) [Action n | n <- [0..24]]
win_list = [[0,1,2,3],[1,2,3,4]]      -- TODO
lose_list = [[0,1,2],[1,2,3],[2,3,4]] -- TODO

squava :: Game
squava move (State (mine,others) available) 
    | win move mine                = EndOfGame 1       -- agent wins
    | available == [move]          = EndOfGame 0       -- no more moves, tie
    | otherwise                    =
          ContinueGame (State (others,(move:mine))   -- note roles have flipped
                        [act | act <- available, act /= move])

-- win n ns = returns -1 if current player lost, 1 if won, 0 if neither
game_end :: Action -> [Action] -> Int
game_end n ns
  | win n ns = 1
  | lose n ns = -1
  | otherwise = 0

win (Action n) ns = or [foldr (\x y -> y && elem (Action x) ns) True (delete n lst) | lst <- filter (elem n) win_list]

lose (Action n) ns = or [foldr (\x y -> y && elem (Action x) ns) True (delete n lst) | lst <- filter (elem n) lose_list]

delete x lst = filter (/= x) lst


--convertToCoord :: Action -> (Int, Int)

{-
0  1  2  3  4
5  6  7  8  9
10 11 12 13 14
15 16 17 18 19
20 21 22 23 24 
-}
-- filter out winning boards with recent move

-- game_end (Action 0) [(Action 1),(Action 2),(Action 3)]
-- game_end (Action 0) [(Action 1),(Action 2)]
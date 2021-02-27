module Squava where

import Helper
import Hash

-- Borrowed and built upon the CPSC 312 MagicSum_Ord.hs

data State = State InternalState [Action]   -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame State Int           -- end of game: ending state, value
            | ContinueGame State            -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

-- Squava Implementation
  --28 ways to win by connecting 4 in a row
  --46 ways to lost by connecting 3 in a row

{- Board positions represented in integers
  0  1  2  3  4
  5  6  7  8  9
  10 11 12 13 14
  15 16 17 18 19
  20 21 22 23 24 
-}

data Coord = Coord String
    deriving (Ord, Eq)


instance Read Coord where
    readsPrec i st = [(Coord c, rst) | (c, rst) <- readsPrec i st]


instance Show Coord where
  show (Coord c) = show c

data Action = Action Int                   -- player moves are ints representing position
         deriving (Ord,Eq)
type InternalState = ([Action],[Action])   -- (self,other) actions done by P1 and P2

instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]

instance Hash State where
   hash (State (a1,a2) a3) = hash (a1++a2++a3)
instance Hash Action where
   hash (Action a) = hash a

{- Short cuts -}
a x = (Action x)
as lst = [Action i | i <- lst]
actionToInt (Action x) = x
resToState (ContinueGame s) = s

{- Constants -}
squava_start = State ([],[]) [Action n | n <- [0..24]] -- starting state
--all possible lists of 4
win_list = [[0,1,2,3], [1,2,3,4], [5,6,7,8], [6,7,8,9], [10,11,12,13], [11,12,13,14], [15,16,17,18], [16,17,18,19], [20,21,22,23], [21,22,23,24], [0,5,10,15], [5,10,15,20], [1,6,11,16],[6,11,16,21], [2,7,12,17], [7,12,17,22], [3,8,13,18], [8,13,18,23], [4,9,14,19], [9,14,19,24], [0,6,12,18], [6,12,18,24], [1,7,13,19], [5,11,17,23], [9,13,17,21], [8,12,16,20], [4,8,12,16], [3,7,11,15]]    
--all possible lines of three
lose_list = [[0,1,2], [1,2,3], [2,3,4], [5,6,7], [6,7,8], [7,8,9], [10,11,12], [11,12,13], [12,13,14], [15,16,17], [16,17,18], [17,18,19], [20,21,22], [22,23,24], [0,5,10], [5,10,15], [10,15,20], [1,6,11], [6,11,16], [11,16,21], [2,7,12],[7,12,17], [12,17,22], [3,8,13], [8,13,18], [13,18,23], [4,9,14], [9,14,19], [14,19,24], [7,11,15], [3,7,11], [12,16,20], [8,12,16], [4,8,12], [13,17,21], [9,13,17], [1,7,13], [13,17,19], [0,6,12], [6,12,18], [12,18,24], [5,11,17], [11,17,23], [2,6,10], [2,8,14], [14,18,22], [10,16,22]]
--corners of 4x4 squares (for heuristic evaluation)
win_square = [[0, 3, 15, 18], [5, 8, 20, 23], [1, 4, 16, 19], [6, 9, 21, 24]]
ts = (State ((as [0,3,5,11,15,19,23]),(as [1,4,7,8,10,16,17])) (as [2,6,9,12,13,14,18,20,21,22,24]))

-- squava move State returns result after move is made
squava :: Game
squava move (State (mine,others) available) 
    | ge /= 0                      = EndOfGame next_state ge -- agent has won or lost
    | available == [move]          = EndOfGame next_state 0  -- no more moves, tie
    | otherwise                    =
          ContinueGame next_state
      where ge = game_end move mine
            next_state = (State (others,(move:mine))
                        (delete move available))

-- inserts into sorted list, for hashing
squava_ord :: Game
squava_ord move (State (mine,others) available) 
    | ge /= 0                      = EndOfGame next_state ge -- agent has won or lost
    | available == [move]          = EndOfGame next_state 0  -- no more moves, tie
    | otherwise                    =
          ContinueGame next_state
      where ge = game_end move mine
            next_state = (State (others,(insert move mine))
                        (delete move available))

-- win n ns = returns -1 if current player has lost given move, 1 if won, 0 if neither
game_end :: Action -> [Action] -> Int
game_end n ns
  | checkend n ns win_list = 1000
  | checkend n ns lose_list = -1000
  | otherwise = 0

-- returns if action list matches a line in given game_lst
checkend (Action n) ns game_lst = or [all (\x -> x==n || (elem (Action x) ns)) lst | lst <- game_lst, elem n lst]
{- 
  > game_end (a 0) (as [1,2,3])
  1000

  > game_end (a 0) (as [1,2])
  -1000

  > game_end (a 0) (as [1])
  0
-}

-- evaluates heuristic value of given State for P2
state_value (EndOfGame s val) = val
state_value (ContinueGame (State (p1, p2) avail)) = (state_value_helper p2) - (state_value_helper p1)

state_value_helper moves = foldr (\x prev -> prev + (square_sum moves x)) 0 win_square

square_sum p1 square = 
  heuristic_square (foldr (\x y -> if elem (Action x) p1 then (y+1) else y) 0 square)

heuristic_square :: Int -> Int
heuristic_square num
  | num == 0 = 0
  | num == 1 = 1
  | num == 2 = 10
  | num == 3 = 125
  | otherwise = 200
  
-- inserts value into ordered list
insert :: Ord a => a -> [a] -> [a]
insert e [] = [e]
insert e (h:t)
  | e <= h = (e:h:t)
  | otherwise = h: (insert e t)
module Squava where

import Helper

-- Borrowed and built upon the CPSC 312 MagicSum.hs

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame State Double        -- end of game: heuristic value, starting state
            | ContinueGame State            -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

-- Squava Implementation
  --28 ways to win
  --42 OR 46 ways to lost by connecting 3 in a row

{- Board positions represented in integers
  0  1  2  3  4
  5  6  7  8  9
  10 11 12 13 14
  15 16 17 18 19
  20 21 22 23 24 
-}

data Action = Action Int                   -- player moves are ints representing position
         deriving (Ord,Eq)
type InternalState = ([Action],[Action])   -- (self,other) actions done by P1 and P2

instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]

a x = (Action x)                  -- short cuts
as lst = [Action i | i <- lst]     

{- Constants -}
squava_start = State ([],[]) [Action n | n <- [0..24]] -- starting state
--all possible lists of 4
win_list = [[0,1,2,3], [1,2,3,4], [5,6,7,8], [6,7,8,9], [10,11,12,13], [11,12,13,14], [15,16,17,18], [16,17,18,19], [20,21,22,23], [21,22,23,24], [0,5,10,15], [5,10,15,20], [1,6,11,16],[6,11,16,21], [2,7,12,17], [7,12,17,22], [3,8,13,18], [8,13,18,23], [4,9,14,19], [9,14,19,24], [0,6,12,18], [6,12,18,24], [1,7,13,19], [5,11,17,23], [21,17,13,9], [20,16,12,8], [16,12,8,4], [15,11,7,3]]    
--all possible lines of three 
lose_list = [[0,1,2], [1,2,3], [2,3,4], [5,6,7], [6,7,8], [10,11,12], [11,12,13], [12,13,14], [15,16,17], [16,17,18], [17,18,19], [20,21,22], [22,23,24], [0,5,10], [5,10,15], [10,15,20], [1,6,11], [6,11,16], [11,16,21], [2,7,12],[7,12,17], [12,17,22], [3,8,13], [8,13,18], [13,18,23], [4,9,14], [9,14,19], [14,19,24], [15,11,7], [11,7,3], [20,16,12], [16,12,8], [12,8,4], [21,17,13], [17,13,9], [1,7,13], [17,13,19], [0,6,12], [6,12,18], [12,18,24], [5,11,17], [11,17,23], [10,6,2], [2,8,14], [14,18,22], [22,16,10]]

-- squava move State returns result after move is made
squava :: Game
squava move (State (mine,others) available) 
    | ge /= 0                      = EndOfGame next_state (fromIntegral ge) -- agent has won or lost
    | available == [move]          = EndOfGame next_state 0 -- no more moves, tie
    | otherwise                    =
          ContinueGame next_state
      where ge = game_end move mine
            next_state = (State (others,(move:mine))
                        (delete move available))

-- win n ns = returns -1 if current player has lost given move, 1 if won, 0 if neither
game_end :: Action -> [Action] -> Int
game_end n ns
  | checkend n ns win_list = 1
  | checkend n ns lose_list = -1
  | otherwise = 0

-- returns if action list matches a line in given game_lst
checkend (Action n) ns game_lst = or [all (\x -> elem (Action x) ns) (delete n lst) | lst <- game_lst, elem n lst]

{- 
  > game_end (a 0) (as [1,2,3])
  1

  > game_end (a 0) (as [1,2])
  -1

  > game_end (a 0) (as [1])
  0
-}

res_to_state (ContinueGame s) = s


{- win and lose lists as lists of Action
win_list = [[(Action 0),(Action 1),(Action 2),(Action 3)],[(Action 1),(Action 2),(Action 3),(Action 4)], [(Action 5),(Action 6),(Action 7),(Action 8)], [(Action 6),(Action 7),(Action 8),(Action 9)], [(Action 10),(Action 11),(Action 12),(Action 13)], [(Action 11),(Action 12),(Action 13),(Action 14)], [(Action 15),(Action 16),(Action 17),(Action 18)], [(Action 16),(Action 17),(Action 18),(Action 19)], [(Action 20),(Action 21),(Action 22),(Action 23)], [(Action 21),(Action 22),(Action 23),(Action 24)], [(Action 0),(Action 5),(Action 10),(Action 15)], [(Action 5),(Action 10),(Action 15),(Action 20)], [(Action 1),(Action 6),(Action 11),(Action 16)],[(Action 6),(Action 11),(Action 16),(Action 21)], [(Action 2),(Action 7),(Action 12),(Action 17)], [(Action 7),(Action 12),(Action 17),(Action 22)], [(Action 3),(Action 8),(Action 13),(Action 18)], [(Action 8),(Action 13),(Action 18),(Action 23)], [(Action 4),(Action 9),(Action 14),(Action 19)], [(Action 9),(Action 14),(Action 19),(Action 24)], [(Action 0),(Action 6),(Action 12),(Action 18)], [(Action 6),(Action 12),(Action 18),(Action 24)], [(Action 1),(Action 7),(Action 13),(Action 19)], [(Action 5),(Action 11),(Action 17),(Action 23)], [(Action 21),(Action 17),(Action 13),(Action 9)], [(Action 20),(Action 16),(Action 12),(Action 8)], [(Action 16),(Action 12),(Action 8),(Action 4)], [(Action 15),(Action 11),(Action 7),(Action 3)]]    
--all possible lines of three 
lose_list = [[(Action 0),(Action 1),(Action 2)],[(Action 1),(Action 2),(Action 3)],[(Action 2),(Action 3),(Action 4)], [(Action 5),(Action 6),(Action 7)], [(Action 6),(Action 7),(Action 8)],[(Action 10),(Action 11),(Action 12)], [(Action 11),(Action 12),(Action 13)], [(Action 12),(Action 13),(Action 14)], [(Action 15),(Action 16),(Action 17)], [(Action 16),(Action 17),(Action 18)], [(Action 17),(Action 18),(Action 19)], [(Action 20),(Action 21),(Action 22)], [(Action 22),(Action 23),(Action 24)], [(Action 0),(Action 5),(Action 10)], [(Action 5),(Action 10),(Action 15)], [(Action 10),(Action 15),(Action 20)], [(Action 1),(Action 6),(Action 11)], [(Action 6),(Action 11),(Action 16)], [(Action 11),(Action 16),(Action 21)], [(Action 2),(Action 7),(Action 12)],[(Action 7),(Action 12),(Action 17)], [(Action 12),(Action 17),(Action 22)], [(Action 3),(Action 8),(Action 13)], [(Action 8),(Action 13),(Action 18)], [(Action 13),(Action 18),(Action 23)], [(Action 4),(Action 9),(Action 14)], [(Action 9),(Action 14),(Action 19)], [(Action 14),(Action 19),(Action 24)], [(Action 15),(Action 11),(Action 7)], [(Action 11),(Action 7),(Action 3)], [(Action 20),(Action 16),(Action 12)], [(Action 16),(Action 12),(Action 8)], [(Action 1),(Action 8),(Action 4)], [(Action 21),(Action 17),(Action 13)], [(Action 17),(Action 13),(Action 9)], [(Action 1),(Action 7),(Action 13)], [(Action 17),(Action 13),(Action 19)], [(Action 0),(Action 6),(Action 12)], [(Action 6),(Action 12),(Action 8)], [(Action 12),(Action 18),(Action 24)], [(Action 5),(Action 11),(Action 17)], [(Action 11),(Action 17),(Action 23)], [(Action 10),(Action 6),(Action 2)], [(Action 2),(Action 8),(Action 14)], [(Action 14),(Action 18),(Action 22)], [(Action 22),(Action 16),(Action 10)]]
-}
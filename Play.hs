module Play where

import Squava
import Minimax
import System.IO
import Text.Read   (readMaybe)

start = 
  do
    putStrLn "Player options are: 0=Human, 1=Minimax"
    putStrLn "What is Player 1?"
    p1 <- getLine
    putStrLn "What is Player 2?"
    p2 <- getLine
    if (isValidOption p1) && (isValidOption p2)
      then
        play squava (ContinueGame squava_start) True (read p1) (read p2)
      else
        do 
          putStrLn "Invalid Options"
          start

play :: Game -> Result -> Bool -> Int -> Int -> IO ()
play game (ContinueGame state) isP1 p1 p2 =
  do
  display isP1 state
  if p1 == 0
    then
      person_play game (ContinueGame state) (not isP1) p2 p1
    else 
      minimax game (ContinueGame state) (not isP1) p2 p1

play game (EndOfGame state v) isP1 p1 p2 =
  do 
    display isP1 state
    putStrLn "End of Game!"
    start

person_play game (ContinueGame state) isP1 p1 p2 =
   do
      let State internal avail = state
      putStrLn ("State: "++show internal++" choose one of "++show avail)
      line <- getLine
      case (readMaybe line :: Maybe Action) of
        Nothing ->
           person_play game (ContinueGame state) isP1 p1 p2
        Just action ->
           if (action `elem` avail)
             then
                play game (game action state) isP1 p1 p2
             else
               do
                putStrLn ("Illegal move: "++ show action)
                person_play game (ContinueGame state) isP1 p1 p2

minimax game (ContinueGame state) isP1 p1 p2 =
  do
    putStrLn ("The computer chose ")
    person_play game (ContinueGame state) isP1 p1 p2


isValidOption o = elem o ["0","1"]

display :: Bool -> State -> IO ()
display isP1 (State (p1,p2) avail) = do
  let newinternal = if isP1 then (p1,p2) else (p2,p1)
  putStrLn "---------------------"
  putStrLn (display_row newinternal 0) 
  putStrLn "---------------------"
  putStrLn (display_row newinternal 5)
  putStrLn "---------------------"
  putStrLn (display_row newinternal 10)
  putStrLn "---------------------"
  putStrLn (display_row newinternal 15)
  putStrLn "---------------------"
  putStrLn (display_row newinternal 20)
  putStrLn "---------------------"

--(Action a) => ([a],[a]) -> Int -> String
display_row :: ([Action],[Action]) -> Int -> String
display_row (p1,p2) offset = display_cell p1 p2 offset 0

display_cell _ _ _ 5 = "|"
display_cell p1 p2 pos col
  | elem (a pos) p1 = "| X " ++ next
  | elem (a pos) p2 = "| O " ++ next
  | otherwise = "|   " ++ next
  where next = display_cell p1 p2 (pos+1) (col+1)
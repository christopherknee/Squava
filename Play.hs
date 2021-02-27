module Play where

import Squava
import Helper
import Minimax
import AlphaBeta
import AlphaBetaMem
import System.IO
import Text.Read   (readMaybe)

--Prompts the user to select the first players type and then prompts for selection of the second player
start = 
  do
    putStrLn "Player options are: 0=Human, 1=Minimax, 2=AlphaBeta"
    putStrLn "What is Player 1?"
    p1 <- getLine
    if not (isValidOption p1)
      then 
        do
          putStrLn "Invalid Option, please choose again"
          start 
      else 
        onePlayerSelected p1

onePlayerSelected p1 =
  do
    putStrLn "Player options are: 0= Human, 1=Minimax, 2=AlphaBeta,3=AlphaBetaMem"
    putStrLn "What is Player 2?"
    p2 <- getLine 
    if not (isValidOption p2)
      then
        do
          putStrLn "Invalid Option, please choose again"
          onePlayerSelected p1
      else
        play squava (ContinueGame squava_start) True (read p1) (read p2)

--If presented with an end state, play presents the end message, otherwise calls the corresponding player's play function
play :: Game -> Result -> Bool -> Int -> Int -> IO ()
play game (ContinueGame state) isP1 p1 p2 =
  do
    display isP1 state
    if p1 == 0
      then
        person_play game (ContinueGame state) (not isP1) p2 p1
      else
        if p1 == 1
          then  
            minimax_play game (ContinueGame state) (not isP1) p2 p1
          else
            if p1 == 2
              then  
                alphabeta_play game (ContinueGame state) (not isP1) p2 p1
              else
                alphabetamem_play game (ContinueGame state) (not isP1) p2 p1

play game (EndOfGame state v) isP1 p1 p2 =
  do 
    display isP1 state
    putStrLn "End of Game!"
    if (v == 1000) 
      then
        if (isP1) then putStrLn "Player 2 wins!" else putStrLn "Player 1 wins!"
      else
        if (isP1) then putStrLn "Player 1 wins!" else putStrLn "Player 2 wins!"
    playAgain

playAgain = 
  do
    putStrLn "Play again? 1 = Yes, 0 = No"
    answer <- getLine
    if not (isValidPlayAgain answer)
      then
        do
          putStrLn "Invalid Option, please choose again"
          playAgain
      else
        if (answer == "1") then start else putStrLn "Goodbye!"

-- person_play updates the game state based on human input
person_play game (ContinueGame state) isP1 p1 p2 =
   do
      let State internal avail = state
      putStrLn ("Please select a coordinate to make your move")
      line <- getLine
      case (coordToInt line) of
        Nothing ->
            do
                putStrLn ("Illegal input")
                person_play game (ContinueGame state) isP1 p1 p2
        Just val ->
            do
                let action = (Action val)
                if (action `elem` avail)
                    then
                        play game (game action state) isP1 p1 p2
                else
                    do
                        putStrLn ("Illegal move")
                        person_play game (ContinueGame state) isP1 p1 p2

-- minimax_play updates the game state based on Minimax algorithm (depth=2)
minimax_play game (ContinueGame state) isP1 p1 p2 =
  let 
    opponent_move = mm_player 2 game state
  in
    do
      putStrLn ("The computer chose "++(intToCoord (actionToInt opponent_move)))
      play game (game opponent_move state) isP1 p1 p2

-- alphabeta_play updates the game state based on Alphabeta algorithm (depth=4)
alphabeta_play game (ContinueGame state) isP1 p1 p2 =
  let 
    opponent_move = ab_player 4 game state
  in
    do
      putStrLn ("The computer chose "++(intToCoord (actionToInt opponent_move)))
      play game (game opponent_move state) isP1 p1 p2

-- alphabeta_play updates the game state based on Alphabeta Memoized algorithm (depth=2)
alphabetamem_play game (ContinueGame state) isP1 p1 p2 =
  let 
    opponent_move = abm_player 2 game state
  in
    do
      putStrLn ("The computer chose "++(intToCoord (actionToInt opponent_move)))
      play game (game opponent_move state) isP1 p1 p2

--Checks to make sure that the selected player type are valid choices
isValidOption o = elem o ["0","1","2","3"]

isValidPlayAgain o = elem o ["0", "1"]

--Display presents the current game state to the console
display :: Bool -> State -> IO ()
display isP1 (State (p1,p2) avail) = do
  let newinternal = if isP1 then (p1,p2) else (p2,p1)
  putStrLn "    A   B   C   D   E"
  putStrLn "   ---------------------"
  putStrLn (" 1 " ++ (display_row newinternal 0))
  putStrLn "   ---------------------"
  putStrLn (" 2 " ++ (display_row newinternal 5))
  putStrLn "   ---------------------"
  putStrLn (" 3 " ++ (display_row newinternal 10))
  putStrLn "   ---------------------"
  putStrLn (" 4 " ++ (display_row newinternal 15))
  putStrLn "   ---------------------"
  putStrLn (" 5 " ++ (display_row newinternal 20))
  putStrLn "   ---------------------"

--Display_row displays a single row of the game board
display_row :: ([Action],[Action]) -> Int -> String
display_row (p1,p2) offset = display_cell p1 p2 offset 0

--display_cell displays a single cell of the game board
display_cell _ _ _ 5 = "|"
display_cell p1 p2 pos col
  | elem (a pos) p1 = "| X " ++ next
  | elem (a pos) p2 = "| O " ++ next
  | otherwise = "|   " ++ next
  where next = display_cell p1 p2 (pos+1) (col+1)
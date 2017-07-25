module TTTAI(
    aiMove
)where
------------------
import GeneralGame
import TicTacToeBoardInstance
import GeneralBoardFunctions
import Data.Char (ord, chr)
------------------
-- ############ --
------------------
aiMove :: Board Mark -> Bool -> String
aiMove inBoard aiPlayCross = fst $ bestMove inBoard aiPlayCross



bestMove :: Board Mark -> Bool -> (String,Int)
bestMove evalBoard aiPlayCross = foldl1 (\x acc -> if snd acc > snd x then acc else x) $ assignPoints evalBoard aiPlayCross (emptyPositions evalBoard)

assignPoints :: Board Mark -> Bool -> [String] ->  [(String, Int)]
assignPoints evalBoard aiPlayCross [] = []
assignPoints evalBoard aiPlayCross (firstMove:possibleMovesLeft) =
    [(firstMove, assignPointsPos evalBoard aiPlayCross firstMove)] ++
    assignPoints evalBoard aiPlayCross (possibleMovesLeft)

assignPointsPos :: Board Mark -> Bool -> String -> Int
assignPointsPos evalBoard aiPlayCross move
    | gameIsATie $ modBoardAccordInput evalBoard move aiPlayCross = 0
    | possibleToWin evalBoard aiPlayCross move = evalPosition move + 100
    | otherwise = evalPosition move - snd (bestMove (modBoardAccordInput evalBoard move aiPlayCross) (aiPlayCross == False))

evalPosition :: String -> Int
evalPosition move
    | move == "B2"    = 3
    | move == "B1" || move == "A2" || move == "C2" || move == "B3"    = 1
    | otherwise       = 2

possibleToWin evalBoard aiPlayCross move
    | currentPlayerHasWon (modBoardAccordInput evalBoard move aiPlayCross) = True
    | otherwise = False

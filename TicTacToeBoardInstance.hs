module TicTacToeBoardInstance
(
  Mark(Circle,Cross,Empty),
  modBoardAccordInput
)where
-----------------
import Data.Char (ord, chr)
import GeneralGame
import CoordinateFunctions
import GeneralBoardFunctions
------------------
-- ############ --
------------------


data Mark = Circle | Cross | Empty deriving Eq
instance Show Mark where
    show Circle = "O"
    show Cross = "X"
    show Empty = " "



instance BoardGame Mark where
    newInputBoard = newInputBoard''
    startBoard = emptyBoard
    currentPlayerHasWon = areThereThreeInARow
    gameIsATie = isThereATie
    printBoard currentBoard = print (currentBoard)
    empty = Empty
    isWhite x = (x ==Cross)

--------------------
-- ############## --
--------------------
newInputBoard'' :: Board Mark -> Bool -> Int -> (Board Mark ->  Bool -> String) -> IO (Board Mark)
newInputBoard'' currentBoard itsCrossTurn mainChoise aiMove = do
    inputLine <- (if mainChoise == 1 || (itsCrossTurn && mainChoise == 2) || (itsCrossTurn ==False && mainChoise == 3) then
            getLine
        else
            return $ aiMove currentBoard itsCrossTurn)
    let newBoard = if inputLine == "q" then
          deadBoard
        else
          modBoardAccordInput currentBoard inputLine itsCrossTurn
    return newBoard

------------------------
emptyBoard = Board [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]
------------------------

isThereATie :: Board Mark -> Bool
isThereATie (Board mList)
    | areThereThreeInARow (Board mList) = False
    | all (all (/=Empty) ) mList = True
    | otherwise = False

-----------------

areThereThreeInARow :: Board Mark -> Bool
areThereThreeInARow (Board mList) =
        areThreeEq x1 x2 x3 ||
        areThreeEq x4 x5 x6 ||
        areThreeEq x7 x8 x9 ||
        areThreeEq x1 x4 x7 ||
        areThreeEq x2 x5 x8 ||
        areThreeEq x3 x6 x9 ||
        areThreeEq x1 x5 x9 ||
        areThreeEq x7 x5 x3
        where
           x1 = mList !! 0 !! 0
           x2 = mList !! 0 !! 1
           x3 = mList !! 0 !! 2
           x4 = mList !! 1 !! 0
           x5 = mList !! 1 !! 1
           x6 = mList !! 1 !! 2
           x7 = mList !! 2 !! 0
           x8 = mList !! 2 !! 1
           x9 = mList !! 2 !! 2

areThreeEq :: Mark -> Mark -> Mark -> Bool
areThreeEq Empty _ _ = False
areThreeEq _ Empty _ = False
areThreeEq _ _ Empty = False
areThreeEq x y z = (x==y) && (y == z)

-----------------
-- Unless legal input, the same board is returned
-- Notice that we can not add a mark on a non-empty
-- place --
modBoardAccordInput :: Board Mark -> String -> Bool -> Board Mark
modBoardAccordInput inBoard input itsCrossTurn
    | isInputLegal inBoard input = Board (modBoard' (convToXCord input) (convToYCord input) inBoard itsCrossTurn)
    | otherwise = inBoard

modBoardAccordInput' x y inBoard itsCrossTurn
    | x < 0 || y<0 || x>2 || y>2 = inBoard
    | otherwise = Board (modBoard' x y inBoard itsCrossTurn)

modBoard' x y (Board markList) itsCrossTurn
     | y == 0  = [modBoard'' x (head markList) itsCrossTurn] ++ tail markList
     | otherwise = [head markList] ++ modBoard' x (y-1) (Board (tail markList)) itsCrossTurn

modBoard'' x markList itsCrossTurn
     | x == 0 && head markList /= Empty = markList
     | x == 0 && itsCrossTurn = [Cross] ++ tail markList
     | x == 0                 = [Circle] ++ tail markList
     | otherwise              = [head markList] ++ (modBoard'' (x-1) (tail markList) itsCrossTurn)



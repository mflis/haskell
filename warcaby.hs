import Debug.Trace
import Data.Maybe
import Data.Tree


data Field = White | WhiteQueen | Black | BlackQueen | Empty  
 deriving (Eq)

data Board = Board [[ Maybe Field]] deriving (Eq)

type Position = (Int,Int)

-- positions od possible move with board after making that move
type PossibleMove = (Position,Board)

readField :: Char -> Maybe Field

readField char 
 | char == 'b' = Just Black
 | char == 'B' = Just BlackQueen
 | char == 'w' = Just White
 | char == 'W' = Just WhiteQueen
 | char == '-' = Just Empty
 | otherwise = error "parse error"

instance Show Field where
   show Black = "b"
   show BlackQueen = "B"
   show White = "w"
   show WhiteQueen = "W"
   show Empty = "-"

printField:: Maybe Field -> Char
printField (Just field) = head $ show field

printOneLine :: [Maybe Field] -> [Char]
printOneLine line  =  (map printField  line ) ++ "\n"

instance Show Board where
 show board@(Board listOfLists) = "\n" ++ (concat $ map printOneLine listOfLists)
 



mapListOfFields singleLineString = map readField singleLineString
makeBoardFromString :: String -> Board
makeBoardFromString stringRepresentation = 
 Board $ map mapListOfFields (words stringRepresentation) 

-------------------------------------------------------



-------------------------------------------------------------------------
getLegalPositions rows = 
 [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]


--	USING !! OPERATOR LISTS ARE 0 - INDEXED


getFigureAtPosition ::   Board ->  Position -> Maybe Field
getFigureAtPosition board@(Board listOfLists)  position@(x,y) 
 | 0 < x &&  x <= length listOfLists && 0 < y 
   && y <= length listOfLists  = (listOfLists !! (y-1)) !! (x-1)
 | otherwise = Nothing



changeFigureInLine :: Int ->  [Maybe Field] -> Maybe Field -> [Maybe Field]
changeFigureInLine positionInLine line newFigure =  
 let (firstPart,secondPart) = splitAt positionInLine  line   
 in   (trimLastElement firstPart) ++ [newFigure] ++ secondPart

--workaround because init on empty list throws exception
trimLastElement:: [a] -> [a]
trimLastElement [] = []
trimLastElement list = init list 

changeFigureInPosition :: Position -> Maybe Field -> Board -> Board
changeFigureInPosition  position@(x,y) newFigure (Board listOfLists) = 
 let (firstPart,secondPart) = splitAt y listOfLists   in    
 Board((trimLastElement firstPart) ++ [changedLine]  ++ secondPart)
 where changedLine =  changeFigureInLine x (listOfLists !! (y-1)) newFigure


moveFigure :: Position -> Position -> Board -> Board
moveFigure oldPosition newPosition board = 
 let removedOldPosition = changeFigureInPosition oldPosition (Just Empty) board
 in  changeFigureInPosition newPosition oldFigure removedOldPosition
 where oldFigure = getFigureAtPosition  board oldPosition 


makeCapture :: Position -> Position -> Board -> Board
makeCapture startField fieldToBeCaptured board = 
 boardAfterMove
  where
   figureToMove = getFigureAtPosition board startField
   newPosition = getPositionAfterCapture startField fieldToBeCaptured
   boardWithoutCapturedPosition = changeFigureInPosition fieldToBeCaptured (Just Empty) board
   boardAfterMove = moveFigure startField newPosition boardWithoutCapturedPosition




getPosition ::  Board -> Position  -> Maybe Position
getPosition  board@(Board listOfLists)  position@(x,y)
 | 0 < x &&  x <= length listOfLists && 0 < y 
   && y <= length listOfLists  = Just position
 | otherwise = Nothing


 

getMovesForBlack::Board -> Position -> Maybe Field -> [Position]
getMovesForBlack board position@(x,y) figure  =
 filter ( filtering board) . map fromJust .
    filter (/=Nothing) $ map (getPosition board) listOfMoves 
 where 
  filtering 
   | figure == Just Black || figure == Just BlackQueen = isNonBlack
   | figure == Just White || figure == Just WhiteQueen = isNonWhite
  listOfMoves 
   | figure == Just Black = [(x+1,y+1),(x-1,y+1)] 
   | figure == Just White = [(x+1,y-1),(x-1,y-1)]
   | otherwise =   [(x+1,y-1),(x-1,y-1),(x+1,y+1),(x-1,y+1)] -- white or black Queen




isNonBlack :: Board -> Position -> Bool
isNonBlack board position =
 (figure /= Just Black) && (figure /= Just BlackQueen)
 where
  figure = getFigureAtPosition board position
 
isNonWhite :: Board -> Position -> Bool
isNonWhite board position = 
 (figure /= Just Black) && (figure /= Just BlackQueen)
  where  
   figure = getFigureAtPosition board position


isEmpty :: Board -> Position -> Bool
isEmpty board position =
 figure == Just Empty
 where  figure =getFigureAtPosition board position 

 
-- list of positions after capturing white figure (only one move forward)
getPossibleCapturesForBlack :: PossibleMove -> Maybe Field -> [PossibleMove]
getPossibleCapturesForBlack move@(position,board) figure =
 listOfPossibleMoves
 where
  filedsWithOppositeColor =  filter (\x -> not $ isEmpty board x) $ getMovesForBlack board position figure
  listOfPositionsToCapture = filter (\pos -> isEmpty board $ getPositionAfterCapture position pos)  filedsWithOppositeColor
  listOfPossibleMoves = map (\pos -> (getPositionAfterCapture position pos,makeCapture position pos board)) listOfPositionsToCapture



getPositionAfterCapture :: Position -> Position -> Position
getPositionAfterCapture startPoint@(xSt,ySt) positionToCapture@(xCap,yCap) =
 (xCap + changeX, yCap + changeY)
 where
  changeX = signum (xCap - xSt)
  changeY = signum (yCap - ySt)


getAllPathsInTree :: Tree a -> [[a]]
getAllPathsInTree (Node label []) = [[label]]
getAllPathsInTree (Node label xs) = map (label:) $ concat $ map getAllPathsInTree xs

getListOfLongestCaptureSequencesForBlack :: Board -> Position -> Maybe Field -> [[PossibleMove]]
getListOfLongestCaptureSequencesForBlack board position figure  
  | maxLengthOfSequence == 1 = [] -- there is only one sequence containing only starting point so there are no capture sequences
  | otherwise =   filter (\x -> (length x) == maxLengthOfSequence) listOfPossibleCaptureSequences 
   where
    treeOfCaptures = unfoldTree (\possibleMove -> (possibleMove, getPossibleCapturesForBlack possibleMove figure)) (position,board)
    listOfPossibleCaptureSequences = getAllPathsInTree treeOfCaptures
    maxLengthOfSequence = maximum $ map length listOfPossibleCaptureSequences

-- returns only the longest sequences of moves
getLegalMoveSequencesForBlack :: Board -> Position -> [[PossibleMove]]
getLegalMoveSequencesForBlack board position
 | figure == Just Empty = error "selected position is empty"
 | captureSequnces == [] = movesOnEmptyFields
 | otherwise =  captureSequnces
  where
   figure = getFigureAtPosition board position
   movesOnEmptyFields = map (\pos -> (pos,moveFigure position pos board): []) $
                       filter (isEmpty board) $ getMovesForBlack board position figure
   captureSequnces = getListOfLongestCaptureSequencesForBlack board position figure
 
 --  not $ position `elem` (getLegalPositions [1..8]) = error "selected position is illegal"

-----------------------------------UNIT TESTS -----------------

initialBoardStr = "-b-b-b-b\nb-b-b-b-\n-b-b-b-b\n--------\n--------\nw-w-w-w-\n-w-w-w-w\nw-w-w-w-"
multiCaptureBlack41Str = "-b-b-b-b\nb-w-w-b-\n--------\n--w-----\n--------\nw-w-w-w-\n-w-w-w-w\nw-w-w-w-"
multiCaptureBlack41 = makeBoardFromString multiCaptureBlack41Str
init2Board = makeBoardFromString "-b-b\nb-b-\n-b-b"
multiBlackQueenCapture44Str = "\n--------\n--------\n--------\n---B----\n--w-w---\n--------\n--w-w---\n--------"
multiBlackQueenCapture44 = makeBoardFromString multiBlackQueenCapture44Str

initBoard = makeBoardFromString initialBoardStr


initialBoardCapture23 = "-b-b-b-b\nb-b-b-b-\n-b-b-b-b\n--w-----\n--------\nw-w-w-w-\n---w-w-w\nw-w-w-w-"


--init23Board = makeBoardFromString initialBoardCapture23

--unitGetPosition1 =  getPosition  initBoard  (1,2) == Just (1,2)
--unitGetPosition2 =  getPosition  initBoard (9,8) == Nothing

--unitGetMovesBlack1 = getMovesForBlack initBoard (2,1) (Just Black)  == []
--unitGetMovesBlack2 = getMovesForBlack initBoard (2,3) (Just Black) == [(3,4),(1,4)]
--unitGetMovesBlack3 = getMovesForBlack initBoard (8,8) (Just Empty)  == []

--unitPossCapturesBlack1 = getPossibleCapturesForBlack init23Board (2,3)


--getLegalMoveSequencesForBlack multiCaptureBlack41 (4,1)
--[[(4,1),(2,3),(4,5)]]



--((4,4),12345678
--________
----------
----------
----------
-----B----
----w-w---
----------
----w-w---
----------
--)
--((6,6),12345678
--________
----------
----------
----------
----------
----w-w---
----------
----w-w-B-
----------
--)
--((4,4),12345678
--________
----------
----------
----------
----------
----w-w---
----------
----w-w-B-
----------
--)
--((6,6),12345678
--________
----------
----------
----------
----------
----w-w---
----------
----w-w---
----------
--)

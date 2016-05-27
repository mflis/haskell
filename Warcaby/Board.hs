
module Board(
Field(White,WhiteQueen,Black,BlackQueen,Empty),
Board,
Position,
getFigureAtPosition,
moveFigure,
makeCapture,
getPosition,
getPositionAfterCapture,
makeBoardFromString
	)
where


import Data.Maybe


data Field = White | WhiteQueen | Black | BlackQueen | Empty  
 deriving (Eq)

data Board = Board [[ Maybe Field]] deriving (Eq)

type Position = (Int,Int)



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

getPositionAfterCapture :: Position -> Position -> Position
getPositionAfterCapture startPoint@(xSt,ySt) positionToCapture@(xCap,yCap) =
 (xCap + changeX, yCap + changeY)
 where
  changeX = signum (xCap - xSt)
  changeY = signum (yCap - ySt)



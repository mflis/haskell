--import Data.Map
import Debug.Trace
import Data.Maybe




data Field = White | WhiteQueen | Black | BlackQueen | Empty deriving Show

data Board = Board [[ Maybe Field]] deriving Show

readField char 
 | char == 'b' = Just Black
 | char == 'B' = Just BlackQueen
 | char == 'w' = Just White
 | char == 'W' =  Just WhiteQueen
 | char == '.' =  Just Empty
 | otherwise = error "parse error"

mapListOfFields singleLineString = map readField singleLineString

makeBoardFromString :: String -> Board
makeBoardFromString stringRepresentation = 
 Board $ map mapListOfFields (words stringRepresentation) 

--indicies  = [[x,y] | x <- "abcdefgh", y <- "12345678"]
--filterNewLines string = Prelude.filter (/='\n') string 

--positions = zip indicies (filterNewLines initialBoardStr)
 

-------------------------------------------------------

initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."


init2Board = makeBoardFromString ".b.b\nb.b.\n.b.b"

getLegalPositions rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]


--	USING !! OPERATOR LISTS ARE 0 - INDEXED

--getFigureAtPosition :: (Int,Int) -> Board -> Maybe Field
getFigureAtPosition position@(x,y) (Board listOfLists) 
 | 0 < x &&  x <= length listOfLists && 0 < y && y <= length listOfLists  =  (listOfLists !! (y-1)) !! (x-1)
 | otherwise = Nothing



--changeFigureInLine :: Int ->  [Field] -> Field -> [Field]
changeFigureInLine positionInLine line newFigure =  
 let (firstPart,secondPart) = splitAt positionInLine  line   
 in   (trimLastElement firstPart) ++ [newFigure] ++ secondPart

--workaround because init on empty list throws exception
trimLastElement [] = []
trimLastElement list = init list 

--changeFigureInPosition :: (Int,Int) -> Field -> Board -> Board
changeFigureInPosition  position@(x,y) newFigure (Board listOfLists) = 
 let (firstPart,secondPart) = splitAt y listOfLists   in    
 Board((trimLastElement firstPart) ++ [changedLine]  ++ secondPart)
 where changedLine =  changeFigureInLine x (listOfLists !! (y-1)) newFigure


changeFigureInPos  position@(x,y) newFigure ( listOfLists) = 
 let (firstPart,secondPart) = splitAt y listOfLists   in   
 (trimLastElement firstPart) ++ [changedLine]  ++ secondPart
 where changedLine = changeFigureInLine x (listOfLists !! (y-1)) newFigure


moveFigure oldPosition newPosition board = 
 let removedOldPosition = changeFigureInPosition oldPosition (Just Empty) board
 in  changeFigureInPosition newPosition oldFigure removedOldPosition
 where oldFigure = getFigureAtPosition oldPosition  board



--getListOfLegalMovesFromPosition :: (Int,Int) -> Board -> [(Int,Int)]



--import Data.Map
import Debug.Trace




data Field = White | WhiteQueen | Black | BlackQueen | Empty deriving Show

data Board = Board [[Field]] deriving Show

readField char 
 | char == 'b' = Black
 | char == 'B' = BlackQueen
 | char == 'w' = White
 | char == 'W' = WhiteQueen
 | char == '.' = Empty
 | otherwise = error "parse error"

mapListOfFields singleLineString = map readField singleLineString

makeBoardFromString :: String -> Board
makeBoardFromString stringRepresentation = Board $ map mapListOfFields (words stringRepresentation) 

--indicies  = [[x,y] | x <- "abcdefgh", y <- "12345678"]
--filterNewLines string = Prelude.filter (/='\n') string 

--positions = zip indicies (filterNewLines initialBoardStr)
 

-------------------------------------------------------

initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."


init2Board = makeBoardFromString ".b.b\nb.b.\n.b.b"

getLegalPositions rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]


--	USING !! OPERATOR LISTS ARE 0 - INDEXED

getFigureAtPosition :: (Int,Int) -> Board -> Field
getFigureAtPosition position@(x,y) (Board listOfLists) = 
 (listOfLists !! (y-1)) !! (x-1)




changeFigureInLine :: Int ->  [Field] -> Field -> [Field]
changeFigureInLine positionInLine line newFigure =  
 let (firstPart,secondPart) = splitAt positionInLine  line   
 in   (trimLastElement firstPart) ++ [newFigure] ++ secondPart

--workaround because init on empty list throws exception
trimLastElement [] = []
trimLastElement list = init list 

--changeFigureInPosition :: (Int,Int) -> Field -> Board -> Board
changeFigureInPosition  position@(x,y) newFigure (Board listOfLists) = 
 let (firstPart,secondPart) = splitAt y listOfLists   in   
 trace ("first: " ++ (show $ trimLastElement firstPart) ++ "\n changed: " ++ (show changedLine)  ++ "\n second: " ++ (show secondPart) ) $ 
 Board((trimLastElement firstPart) ++ [changedLine]  ++ secondPart)
 where changedLine =  changeFigureInLine x (listOfLists !! (y-1)) newFigure


changeFigureInPos  position@(x,y) newFigure ( listOfLists) = 
 let (firstPart,secondPart) = splitAt y listOfLists   in   
 trace ("first: " ++ (show $ trimLastElement firstPart) ++ "\n changed: " ++ (show changedLine)  ++ "\n second: " ++ (show secondPart) ) $ 
 (( trimLastElement firstPart) ++ [changedLine]  ++ secondPart)
 where changedLine =  trace ("line to change: " ++ show(listOfLists !! (y -1))) $ changeFigureInLine x (listOfLists !! (y-1)) newFigure


moveFigure oldPosition newPosition board = 
 let removedOldPosition = changeFigureInPosition oldPosition Empty board
 in  trace ("after removing old: " ++ show removedOldPosition) $ changeFigureInPosition newPosition oldFigure removedOldPosition
 where oldFigure = getFigureAtPosition oldPosition  board

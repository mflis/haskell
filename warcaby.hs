--import Data.Map

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


init2Board = makeBoardFromString ".b.b\n.b.b\n.b.b"

getLegalPositions rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]

--data Position = Position { x:: Int,y :: Int} deriving (Show)   

getFigureAtPosition :: (Int,Int) -> Board -> Field
getFigureAtPosition position@(x,y) (Board listOfLists) = 
 (listOfLists !! y) !! x


changeFigureInLine :: Int ->  [Field] -> Field -> [Field]
changeFigureInLine positionInLine line newFigure =  
 let (firstPart,secondPart) = splitAt positionInLine line   in   (init firstPart) ++ [newFigure] ++ secondPart


changeFigureInPosition :: (Int,Int) -> Field -> Board -> Board
changeFigureInPosition  position@(x,y) newFigure (Board listOfLists) = 
 let (firstPart,secondPart) = splitAt y listOfLists   in   Board((init firstPart) ++ [changedLine]  ++ secondPart)
 where changedLine =  changeFigureInLine x (listOfLists !! y) newFigure

moveFigure oldPosition newPosition board = changeFigureInPosition oldPosition Empty board
 --let removedOldPosition = changeFigureInPosition oldPosition Empty board
 --in changeFigureInPosition newPosition oldFigure removedOldPosition
 --where oldFigure = getFigureAtPosition oldPosition  board

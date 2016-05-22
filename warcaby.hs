import Debug.Trace
import Data.Maybe

data Field = White | WhiteQueen | Black | BlackQueen | Empty 
 deriving (Show, Eq)

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

-------------------------------------------------------



-------------------------------------------------------------------------
getLegalPositions rows = 
 [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]


--	USING !! OPERATOR LISTS ARE 0 - INDEXED

--getFigureAtPosition :: (Int,Int) -> Board -> Maybe Field



getFigureAtPosition ::   Board ->  (Int,Int) -> Maybe Field
getFigureAtPosition board@(Board listOfLists)  position@(x,y) 
 | 0 < x &&  x <= length listOfLists && 0 < y 
   && y <= length listOfLists  = (listOfLists !! (y-1)) !! (x-1)
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
 where oldFigure = getFigureAtPosition  board oldPosition 



--getListOfLegalMovesFromPosition :: (Int,Int) -> Board -> [Maybe(Int,Int)]
--getListOfLegalMovesFromPosition position board
-- | figure == Just Empty = [] 
-- | figure == Just Black = getMovesForBlack board position 
 
---- | figure == Just White = getMovesForWhite position board

-- where figure = getFigureAtPosition  board position

getPosition ::  Board -> (Int,Int)  -> Maybe (Int,Int)
getPosition  board@(Board listOfLists)  position@(x,y)
 | 0 < x &&  x <= length listOfLists && 0 < y 
   && y <= length listOfLists  = Just position
 | otherwise = Nothing


----getMovesForBlack::Board ->(Int,Int)  -> [Maybe(Int,Int)]
--getMovesForBlack board position@(x,y)  =
-- filter ( \x ->  (x /= Black) and (x /= BlackQueen) ) . map (getFigureAtPosition board) .  map (fromJust) filteredFromNothing  
-- where filteredFromNothing = filter (/=Nothing ) $ map (getPosition board) [(x+1,y+1),(x-1,y+1)] 


--getMovesForBlack::Board ->(Int,Int)  -> [(Int,Int)]
getMovesForBlack board position@(x,y)  =
   filter (isNonBlack board) . map fromJust . filter (/=Nothing) $ map (getPosition board) [(x+1,y+1),(x-1,y+1)] 


isNonBlack board position =
 (figure /= Just Black) && (figure /= Just BlackQueen)
 where  figure =getFigureAtPosition board position 


isEmpty board position =
 figure == Just Empty
 where  figure =getFigureAtPosition board position 

--getMovesForWhite::Board ->(Int,Int)  -> [[(Int,Int)]]
--getMovesForWhite position@(x,y) board = 
-- map (getPosition board) [(x+1,y-1),(x-1,y-1)]


--getRecursiveListOfPossibleCapturesBlack board listOfPossibleCaptures =
 

getPossibleCapturesForBlack board position =
 map (getPositionAfterCapture position) whiteFields
 where whiteFields =  filter (\x -> not $ isEmpty board x) $ getMovesForBlack board position 

getPositionAfterCapture startPoint@(xSt,ySt) positionToCapture@(xCap,yCap) =
 (2*xCap - xSt,2*yCap - ySt)
 

-----------------------------------UNIT TESTS -----------------

initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
init2Board = makeBoardFromString ".b.b\nb.b.\n.b.b"

initBoard = makeBoardFromString initialBoardStr


initialBoardCapture23 = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n..w.....\n........\nw.w.w.w.\n...w.w.w\nw.w.w.w."

init23Board = makeBoardFromString initialBoardCapture23

unitGetPosition1 =  getPosition  initBoard  (1,2) == Just (1,2)
unitGetPosition2 =  getPosition  initBoard (9,8) == Nothing

unitGetMovesBlack1 = getMovesForBlack initBoard (2,1)  == []
unitGetMovesBlack2 = getMovesForBlack initBoard (2,3)  == [(3,4),(1,4)]
unitGetMovesBlack3 = getMovesForBlack initBoard (8,8)  == []

unitPossCapturesBlack1 = getPossibleCapturesForBlack init23Board (2,3)



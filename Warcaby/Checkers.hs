module Checkers(
getLegalMoveSequencesForFigure,
getLegalPositions,
PossibleMove,
TypeOfMove(Start,Capture,Move),
initBoard,
minBoard,
multiBlackQueenCapture44,
multiCaptureBlack41
)
where


import Debug.Trace
import Data.Tree
import Data.Maybe
import Board

data TypeOfMove = Move | Capture | Start deriving (Eq,Show)

-- positions od possible move with board after making that move
type PossibleMove = (Position,Board,TypeOfMove)

getLegalPositions rows = 
 [(c, d) | c <- [1..8], d <- rows, or [and [odd c, even d], and [even c, odd d]] ]



-- list of positions after capturing  figure opposite color (only one move forward)
getPossibleCapturesForQueen :: PossibleMove -> Maybe Field  ->  [PossibleMove]
getPossibleCapturesForQueen move@(position,board,_) figure =
  map  (\pos -> (getPositionAfterCapture position pos,makeCapture position pos board,Capture)) positionsToCapture
  where
   positionsToCapture =   map ( !! 0 ) thereIsCapture
   thereIsCapture = filter (isCapture figure) possibilityOfCapture 
   possibilityOfCapture = filter  (\list -> (length list) > 1 ) $ map  dropWhileEmpty rawListOfLists 
   rawListOfLists =  map (getDiagonalInDirection board position) [NorthEast,NorthWest,SouthEast,SouthWest]
   dropWhileEmpty = dropWhile (\pos -> (getFigureAtPosition board pos) == Just Empty )
   isCapture  (Just WhiteQueen) possibleCapture  = (isBlack board (possibleCapture !! 0)) && (isEmpty board (possibleCapture !! 1))  
   isCapture  (Just BlackQueen) possibleCapture  = (isWhite board (possibleCapture !! 0)) && (isEmpty board (possibleCapture !! 1))  

getEmptyMovesForQueen:: PossibleMove -> [PossibleMove]
getEmptyMovesForQueen move@(position,board,_) =
  map  (\pos -> (pos,moveFigure position pos board,Move)) emptyFields  
  where
  emptyFields =  concat . filter  (not . null) $ map  takeWhileEmpty rawListOfLists 
  rawListOfLists =  map (getDiagonalInDirection board position) [NorthEast,NorthWest,SouthEast,SouthWest]
  takeWhileEmpty = takeWhile (isEmpty board)
 

getEmptyMovesForPawn:: PossibleMove -> Maybe Field -> [PossibleMove]
getEmptyMovesForPawn move@(position@(x,y),board,_) figure = 
  map  (\pos -> (pos,moveFigure position pos board,Move)) emptyFields
 where
  emptyFields = filter (isEmpty board) . map fromJust .
    filter isJust $ map (getPosition board) listOfMoves 
  listOfMoves 
   | figure == Just Black = [(x+1,y+1),(x-1,y+1)] 
   | figure == Just White = [(x+1,y-1),(x-1,y-1)]


getPossibleCapturesForPawn :: PossibleMove -> Maybe Field -> [PossibleMove]
getPossibleCapturesForPawn move@(position@(x,y),board,_) figure =
 map (\pos -> (getPositionAfterCapture position pos,makeCapture position pos board,Capture))  fieldsToCapture
 where
  fieldsToCapture =  filter isFieldToJumpEmpty $ filter  filtering $ map fromJust $ filter  isJust $ map (getPosition board)  listOfMoves
  isFieldToJumpEmpty = \pos -> getFigureAtPosition board (getPositionAfterCapture position pos) == Just Empty 
  listOfMoves = [(x+1,y+1),(x-1,y+1), (x+1,y-1),(x-1,y-1)]
  filtering 
   | figure == Just Black = isWhite board 
   | figure == Just White = isBlack board

---- list of positions after capturing  figure opposite color (only one move forward)
--getPossibleCapturesForFigure :: PossibleMove -> Maybe Field -> [PossibleMove]
--getPossibleCapturesForFigure move@(position,board) figure =
-- listOfPossibleMoves
-- where
--  filedsWithOppositeColor =  filter (\x -> not $ isEmpty board x) $ getMovesForFigure board position figure
--  listOfPositionsToCapture = filter (\pos -> isEmpty board $ getPositionAfterCapture position pos)  filedsWithOppositeColor
--  listOfPossibleMoves = map (\pos -> (getPositionAfterCapture position pos,makeCapture position pos board)) listOfPositionsToCapture


  


getAllPathsInTree :: Tree a -> [[a]]
getAllPathsInTree (Node label []) = [[label]]
getAllPathsInTree (Node label xs) = map (label:) $ concat $ map getAllPathsInTree xs

getListOfLongestCaptureSequencesForFigure :: PossibleMove -> Maybe Field -> [[PossibleMove]]
getListOfLongestCaptureSequencesForFigure startMove fig
  | maxLengthOfSequence == 1 = [] -- there is only one sequence containing only starting point so there are no capture sequences
  | otherwise =   filter (\x -> (length x) == maxLengthOfSequence) listOfPossibleCaptureSequences 
   where
    treeOfCaptures
     | (fig == Just Black) || (fig == Just White) = 
           unfoldTree (\possibleMove -> (possibleMove, getPossibleCapturesForPawn possibleMove fig)) startMove
     | (fig == Just BlackQueen) || (fig == Just WhiteQueen) = 
           unfoldTree (\possibleMove -> (possibleMove, getPossibleCapturesForQueen possibleMove fig)) startMove
    listOfPossibleCaptureSequences = getAllPathsInTree treeOfCaptures
    maxLengthOfSequence = maximum $ map length listOfPossibleCaptureSequences

-- returns only the longest sequences of moves
getLegalMoveSequencesForFigure :: Board -> Position -> [[PossibleMove]]
getLegalMoveSequencesForFigure  board position
 | fig == Just Empty = error "selected position is empty"
 | captureSequnces == [] = map (\move -> [startMove,move] ) movesOnEmptyFields
 | otherwise =  captureSequnces
  where
   startMove = (position,board,Start)
   fig = getFigureAtPosition board position
   movesOnEmptyFields 
    | (fig == Just Black) || (fig == Just White) = getEmptyMovesForPawn startMove fig
    | (fig == Just BlackQueen) || (fig == Just WhiteQueen) = getEmptyMovesForQueen startMove
   captureSequnces = getListOfLongestCaptureSequencesForFigure startMove fig





-----------------------------------TEST CASES -----------------

initialBoardStr = "-b-b-b-b\nb-b-b-b-\n-b-b-b-b\n--------\n--------\nw-w-w-w-\n-w-w-w-w\nw-w-w-w-"
initBoard = makeBoardFromString initialBoardStr

minBoardStr =     "----b---\n--------\n--------\n--------\n---w----\n--------\n--------\n--------"
minBoard = makeBoardFromString minBoardStr

multiCaptureBlack41Str = "-b-b-b-b\nb-w-w-b-\n--------\n--w-----\n--------\nw-w-w-w-\n-w-w-w-w\nw-w-w-w-"
multiCaptureBlack41 = makeBoardFromString multiCaptureBlack41Str

init2Board = makeBoardFromString "-b-b\nb-b-\n-b-b"

multiBlackQueenCapture44Str = "\n--------\n--------\n--------\n----B---\n---w-w--\n--------\n---w-w--\n--------"
multiBlackQueenCapture44 = makeBoardFromString multiBlackQueenCapture44Str

initialBoardCapture23 = "-b-b-b-b\nb-b-b-b-\n-b-b-b-b\n--w-----\n--------\nw-w-w-w-\n---w-w-w\nw-w-w-w-"


capturesForBlackQueenStr =  "--------\n--------\n---W----\n--------\n-B------\n--w-----\n--------\n--------"
capturesForBlackQueen = makeBoardFromString capturesForBlackQueenStr
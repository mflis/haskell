import Debug.Trace
import Data.Tree
import Data.Maybe
import Board

-- positions od possible move with board after making that move
type PossibleMove = (Position,Board)

getLegalPositions rows = 
 [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]

getMovesForFigure::Board -> Position -> Maybe Field -> [Position]
getMovesForFigure board position@(x,y) figure  =
 filter (filtering board) . map fromJust .
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
getPossibleCapturesForFigure :: PossibleMove -> Maybe Field -> [PossibleMove]
getPossibleCapturesForFigure move@(position,board) figure =
 listOfPossibleMoves
 where
  filedsWithOppositeColor =  filter (\x -> not $ isEmpty board x) $ getMovesForFigure board position figure
  listOfPositionsToCapture = filter (\pos -> isEmpty board $ getPositionAfterCapture position pos)  filedsWithOppositeColor
  listOfPossibleMoves = map (\pos -> (getPositionAfterCapture position pos,makeCapture position pos board)) listOfPositionsToCapture


getAllPathsInTree :: Tree a -> [[a]]
getAllPathsInTree (Node label []) = [[label]]
getAllPathsInTree (Node label xs) = map (label:) $ concat $ map getAllPathsInTree xs

getListOfLongestCaptureSequencesForFigure :: Board -> Position -> Maybe Field -> [[PossibleMove]]
getListOfLongestCaptureSequencesForFigure board position figure  
  | maxLengthOfSequence == 1 = [] -- there is only one sequence containing only starting point so there are no capture sequences
  | otherwise =   filter (\x -> (length x) == maxLengthOfSequence) listOfPossibleCaptureSequences 
   where
    treeOfCaptures = unfoldTree (\possibleMove -> (possibleMove, getPossibleCapturesForFigure possibleMove figure)) (position,board)
    listOfPossibleCaptureSequences = getAllPathsInTree treeOfCaptures
    maxLengthOfSequence = maximum $ map length listOfPossibleCaptureSequences

-- returns only the longest sequences of moves
getLegalMoveSequencesForFigure :: Board -> Position -> [[PossibleMove]]
getLegalMoveSequencesForFigure board position
 | figure == Just Empty = error "selected position is empty"
 | captureSequnces == [] = movesOnEmptyFields
 | otherwise =  captureSequnces
  where
   figure = getFigureAtPosition board position
   movesOnEmptyFields = map (\pos -> (pos,moveFigure position pos board): []) $
                       filter (isEmpty board) $ getMovesForFigure board position figure
   captureSequnces = getListOfLongestCaptureSequencesForFigure board position figure
 
-----------------------------------TEST CASES -----------------

initialBoardStr = "-b-b-b-b\nb-b-b-b-\n-b-b-b-b\n--------\n--------\nw-w-w-w-\n-w-w-w-w\nw-w-w-w-"
initBoard = makeBoardFromString initialBoardStr

multiCaptureBlack41Str = "-b-b-b-b\nb-w-w-b-\n--------\n--w-----\n--------\nw-w-w-w-\n-w-w-w-w\nw-w-w-w-"
multiCaptureBlack41 = makeBoardFromString multiCaptureBlack41Str

init2Board = makeBoardFromString "-b-b\nb-b-\n-b-b"

multiBlackQueenCapture44Str = "\n--------\n--------\n--------\n---B----\n--w-w---\n--------\n--w-w---\n--------"
multiBlackQueenCapture44 = makeBoardFromString multiBlackQueenCapture44Str

initialBoardCapture23 = "-b-b-b-b\nb-b-b-b-\n-b-b-b-b\n--w-----\n--------\nw-w-w-w-\n---w-w-w\nw-w-w-w-"

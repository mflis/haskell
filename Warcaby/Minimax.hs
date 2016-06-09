import Board
import Checkers
import GameTree
import Data.Maybe
import Data.Ord
import Data.List
import Data.Tree

evaluateBoard :: Board -> Bool -> Int
evaluateBoard board@(Board listOfLists) isBlack 
 | isBlack == True = valueForBlack
 | otherwise = -valueForBlack
 where
 valueForBlack = sum . map getFigureWeight $ concat listOfLists

getFigureWeight :: Maybe Field -> Int
getFigureWeight figure
 | figure == Just Black = 1
 | figure == Just White = -1
 | figure == Just BlackQueen = 3
 | figure == Just WhiteQueen = -3
 | figure == Just Empty = 0


minimax :: Tree GameTreeNode  -> Bool -> Board
minimax gameTree@(Node _ children) isBlack  =
 snd $ maximumBy (comparing $ fst) movesWithEvalutaion
 where 
  movesWithEvalutaion = map (\gameNode@(Node (GameTreeNode (_,_,board) ) _ ) -> (minimizeTree isBlack gameNode,board )) children



maximizeTree :: Bool ->Tree GameTreeNode ->  Int

maximizeTree  isRootBlack (Node game@(GameTreeNode (_,_,board)) [] )  = evaluateBoard board isRootBlack 

maximizeTree isRootBlack (Node _ children)  = maximum . map (minimizeTree isRootBlack) $ children 



minimizeTree :: Bool -> Tree GameTreeNode ->  Int

minimizeTree isRootBlack (Node game@(GameTreeNode(_,_,board)) [] )  = evaluateBoard board isRootBlack 

minimizeTree isRootBlack (Node _ children)  = minimum . map (maximizeTree isRootBlack) $ children 


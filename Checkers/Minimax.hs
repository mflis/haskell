module Minimax(
minimax
)where

import Board
import Checkers
import GameTree
import Data.Maybe
import Data.Ord
import Data.List
import Data.Tree

-------------- evaluate board for white or black player------
------------ (result is always non-negative number) -----------
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

--------------------------------------------------
----------- minimax algorithm --------------------


minimax gameTree@(Node _ children) isBlack  =
  maximumBy (comparing $ fst') movesWithEvalutaion
 where 
  movesWithEvalutaion = map (\gameNode@(Node (GameTreeNode (_,_,board,seq) ) _ ) -> (minimizeTree isBlack gameNode,board, seq )) children


fst' (a,b,c) = a

maximizeTree :: Bool ->Tree GameTreeNode ->  Int

maximizeTree  isRootBlack (Node game@(GameTreeNode (_,_,board,_)) [] )  = evaluateBoard board isRootBlack 

maximizeTree isRootBlack (Node _ children)  = maximum . map (minimizeTree isRootBlack) $ children 



minimizeTree :: Bool -> Tree GameTreeNode ->  Int

minimizeTree isRootBlack (Node game@(GameTreeNode(_,_,board,_)) [] )  = evaluateBoard board isRootBlack 

minimizeTree isRootBlack (Node _ children)  = minimum . map (maximizeTree isRootBlack) $ children 


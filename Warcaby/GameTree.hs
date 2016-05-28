import Board
import Checkers
import Data.Tree

type Depth = Int

-- (how many levels of tree to generate left, is Current Player Black, board
data GameTreeNode = GameTreeNode (Depth,Bool,Board) deriving Show

getAllMoveSequncesForColor :: Board -> Bool -> [[PossibleMove]]
getAllMoveSequncesForColor board isBlack =
 onlyLognest . concat $  map  (getLegalMoveSequencesForFigure board)  positions
 where
 positions | isBlack == True =  getAllBlackPositions board
           | isBlack == False = getAllWhitePositions board


getAllBlackPositions :: Board -> [Position]
getAllBlackPositions board = filter 
 (\pos -> let fig = getFigureAtPosition board pos 
 in  (fig == Just Black) || (fig == Just BlackQueen) ) 
 $ [(x,y)| x<- [1..8], y <- [1..8]]

getAllWhitePositions :: Board -> [Position]
getAllWhitePositions board = filter 
 (\pos -> let fig = getFigureAtPosition board pos 
 in  (fig == Just White) || (fig == Just WhiteQueen) ) 
 $ [(x,y)| x<- [1..8], y <- [1..8]]


onlyLognest :: [[a]] -> [[a]]
onlyLognest listOfLists = 
 filter (\x -> (length x) == maxLengthOfSequence) listOfLists  
 where
 maxLengthOfSequence = maximum $ map length listOfLists



generateGameTreeNodes :: GameTreeNode -> [GameTreeNode]
generateGameTreeNodes (GameTreeNode (howManyLevelsLeft,isBlack,board)) 
 | howManyLevelsLeft == 0 = []
 | otherwise = map  ( \board -> GameTreeNode(depthToGo,oppositeColor,board)) finalBoards
 where
  depthToGo = howManyLevelsLeft -1
  possibleSequnces  = getAllMoveSequncesForColor board isBlack
  oppositeColor = not isBlack
  finalBoards =   map snd $ secureLast possibleSequnces

-- workaround to avoid exceptions on empty lists
secureLast ::Eq a => [[a]] -> [a]
secureLast listOfLists 
 | listOfLists == [[]]  = []
 | otherwise = map last listOfLists


--generateGameTree :: GameTreeNode -> Tree GameTreeNode
generateGameTree initNode = unfoldTree (\node -> (show node, generateGameTreeNodes node)) initNode 


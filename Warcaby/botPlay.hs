import Minimax  
import GameTree
import  Board
import Checkers
import Data.Maybe


loop:: Board -> Bool -> Int -> IO String

loop _ _ 0 = return "Timeout"

loop initBoard isBlack howMany =
    case isWin initBoard of BlackWin -> return "White"
                            WhiteWin -> return "Black"
                            NoWin -> do
                            putStrLn  $ "black? " ++ show isBlack ++  " sequence: " ++ sequence ++ show boardAfterMove
                            loop boardAfterMove (not isBlack) (howMany -1)
 where
 (weight,boardAfterMove,sequence) = minimax tree isBlack
 initNode = GameTreeNode(4,isBlack,initBoard,"")
 tree = generateGameTree initNode


playerVsComputer initBoard isBlack = 
     case isWin initBoard of BlackWin -> return "White Wins"
                             WhiteWin -> return "Black Wins"
                             NoWin ->do
                                putStr "Enter sequence of moves:"
                                sequenceFromUser <-  getLine
                                let afterMove = applyMoveFromString sequenceFromUser isBlack initBoard
                                case afterMove of Nothing -> return "bad sequence"
                                                  otherwise -> do 
                                                                putStrLn $ show afterMove
                                                                playerVsComputer (fromJust afterMove) (not isBlack)
                                
 where
 (weight,boardAfterMove,sequence) = minimax tree isBlack
 initNode = GameTreeNode(4,isBlack,initBoard,"")
 tree = generateGameTree initNode
 


data Win = BlackWin | WhiteWin | NoWin

isWin:: Board -> Win
isWin board 
 |  null $ getAllWhitePositions board = BlackWin
 | null $ getAllBlackPositions board = WhiteWin
 | otherwise = NoWin


applyMoveFromString:: Sequence -> Bool -> Board -> Maybe Board
applyMoveFromString sequence isBlack board 
    | maybeAppliedMove == Nothing = Nothing
    | otherwise = (\(_,board,_) -> Just board) . last $ fromJust maybeAppliedMove 
    where
        moveSequences = getAllMoveSequncesForColor board isBlack
        maybeAppliedMove = listToMaybe $ filter (\moveSeq -> (convertSequenceToString moveSeq) == sequence ) moveSequences


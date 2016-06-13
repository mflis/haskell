import Minimax  
import GameTree
import  Board
import Checkers
import Data.Maybe
import  Control.Monad

data Win = BlackWin | WhiteWin | NoWin deriving Eq


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


playerVsComputer initBoard isBlack = do
    afterUserMove <- userMove initBoard isBlack 
    case afterUserMove of Nothing -> if isWin initBoard /= NoWin 
                                     then  return "the end"
                                     else do putStrLn "bad sequence"
                                             playerVsComputer initBoard isBlack
                          otherwise ->   do afterCompMove <- compMove   (fromJust afterUserMove) (not isBlack) 
                                            case afterCompMove of Nothing -> return "the end"
                                                                  otherwise -> playerVsComputer (fromJust afterCompMove) isBlack
                                                                                                          
                                

--userMove:: Board -> Bool -> IO (Maybe Board)
userMove initBoard isBlack = 
    case isWin initBoard of BlackWin -> do putStrLn "White Wins" ; return Nothing
                            WhiteWin -> do putStrLn "Black Wins" ; return Nothing
                            NoWin -> do
                                        putStr $ "Black: " ++ show isBlack ++  " Enter sequence of moves:"
                                        sequenceFromUser <-  getLine
                                        when (sequenceFromUser == "q") (fail "bye")
                                        let afterUserMove =  applyMoveFromString sequenceFromUser isBlack initBoard
                                        when (afterUserMove /= Nothing) (putStr $ show $ fromJust afterUserMove)
                                        return afterUserMove


--compMove:: Board -> Bool -> IO (Maybe Board)
compMove initBoard isBlack = 
    case isWin initBoard of BlackWin -> do putStrLn "White Wins" ; return Nothing
                            WhiteWin -> do putStrLn "Black Wins" ; return Nothing
                            NoWin ->  do let
                                            node = GameTreeNode(4, isBlack,initBoard ,"")
                                            tree = generateGameTree node
                                            (weight,boardAfterCompMove,sequence) = minimax tree isBlack
                                         putStrLn $ "Opponent move " ++ sequence
                                         putStr $ show boardAfterCompMove
                                         return $ Just boardAfterCompMove



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


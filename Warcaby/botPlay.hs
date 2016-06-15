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

playerVsComputer:: Board -> Bool -> IO String
playerVsComputer initBoard isBlack = do
    afterUserMove <- userMove initBoard isBlack 
    case afterUserMove of Nothing -> if isWin initBoard /= NoWin 
                                     then  return "the end"
                                     else do putStrLn "bad sequence"
                                             playerVsComputer initBoard isBlack
                          otherwise ->   do afterCompMove <- compMove   (fromJust afterUserMove) (not isBlack) 
                                            case afterCompMove of Nothing -> return "the end"
                                                                  otherwise -> playerVsComputer (fromJust afterCompMove) isBlack
                                                                                                          
inputVsComputer:: Board -> Bool -> IO String
-- opponent is playing with white color, and makes starting move
inputVsComputer initBoard False = do
    afterUserMove <- userMoveRaw initBoard False 
    case afterUserMove of Nothing -> if isWin initBoard /= NoWin 
                                     then  return "the end"
                                     else do putStrLn "bad sequence"
                                             inputVsComputer initBoard False
                          otherwise ->   do afterCompMove <- compMoveRaw   (fromJust afterUserMove) True 
                                            case afterCompMove of Nothing -> return "the end"
                                                                  otherwise -> inputVsComputer (fromJust afterCompMove) False    

-- opponent is playing with black color, and computer makes firts move
inputVsComputer initBoard True = do
    afterCompMove <- compMoveRaw initBoard False  
    case afterCompMove of Nothing -> return "the end"
                          otherwise -> do afterUserMove <- userMoveRaw initBoard True 
                                          case afterUserMove of Nothing -> if isWin initBoard /= NoWin 
                                                                           then  return "the end"
                                                                           else do putStrLn "bad sequence"
                                                                                   inputVsComputer initBoard True
                                                                otherwise -> inputVsComputer (fromJust afterUserMove) True    
                          


userMoveRaw:: Board -> Bool -> IO (Maybe Board)
userMoveRaw initBoard isBlack = 
    case isWin initBoard of BlackWin -> do putStrLn "White Wins" ; return Nothing
                            WhiteWin -> do putStrLn "Black Wins" ; return Nothing
                            NoWin -> do
                                        sequenceFromUser <-  getLine
                                        let afterUserMove =  applyMoveFromString sequenceFromUser isBlack initBoard
                                        return afterUserMove


compMoveRaw:: Board -> Bool -> IO (Maybe Board)
compMoveRaw initBoard isBlack = 
    case isWin initBoard of WhiteWin -> do putStrLn "White Wins" ; return Nothing
                            BlackWin -> do putStrLn "Black Wins" ; return Nothing
                            NoWin ->  do let
                                            node = GameTreeNode(4, isBlack,initBoard ,"")
                                            tree = generateGameTree node
                                            (weight,boardAfterCompMove,sequence) = minimax tree isBlack
                                         putStrLn  sequence
                                         return $ Just boardAfterCompMove

userMove:: Board -> Bool -> IO (Maybe Board)
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


compMove:: Board -> Bool -> IO (Maybe Board)
compMove initBoard isBlack = 
    case isWin initBoard of WhiteWin -> do putStrLn "White Wins" ; return Nothing
                            BlackWin -> do putStrLn "Black Wins" ; return Nothing
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


toCheck = makeBoardFromString "--------\n--------\n--------\n--------\n--------\nb-------\n-w------\n--------"
toCheck2 = makeBoardFromString "--------\n--------\n--------\n--------\n--------\nb-------\n-w-w----\n--------"
toCheck3 = makeBoardFromString "--------\n--------\n--------\n--------\n--------\nb---w---\n-w------\n--------"
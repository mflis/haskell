import Minimax  
import GameTree
import  Board
import Checkers
import Data.Maybe
import  Control.Monad

data Win = BlackWin | WhiteWin | NoWin deriving Eq

---------------- HOW TO RUN PROGRAM ----------------
-----------  1'st arg: initial board(initBoard declared in Board.hs)
-----------  2'nd arg: True - user is playing black figures
---------------------- False - user is playing white figures
------------ (user is always making first move, no matter if he's playing black or white figures)
-------------type "q" to force quit the game ---------------------
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
                                                                                                          

userMove:: Board -> Bool -> IO (Maybe Board)
userMove initBoard isBlack = 
    case isWin initBoard of BlackWin -> do putStrLn "White Wins" ; return Nothing
                            WhiteWin -> do putStrLn "Black Wins" ; return Nothing
                            NoWin -> do
                                        putStr " Enter sequence of moves:"
                                        sequenceFromUser <-  getLine
                                        when (sequenceFromUser == "q") (fail "user requested end of game")
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


---------- if move sequence (in PDN format) is correct and legal, then return board after making that move
applyMoveFromString:: Sequence -> Bool -> Board -> Maybe Board
applyMoveFromString sequence isBlack board 
    | maybeAppliedMove == Nothing = Nothing
    | otherwise = (\(_,board,_) -> Just board) . last $ fromJust maybeAppliedMove 
    where
        moveSequences = getAllMoveSequncesForColor board isBlack
        maybeAppliedMove = listToMaybe $ filter (\moveSeq -> (convertSequenceToString moveSeq) == sequence ) moveSequences


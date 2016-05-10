import Data.Map

data Field = White | WhiteQueen | Black | BlackQueen | Empty deriving Show

readField char 
 | char == 'b' = Black
 | char == 'B' = BlackQueen
 | char == 'w' = White
 | char == 'W' = WhiteQueen
 | char == '.' = Empty
 | otherwise = error "parse error"


indicies  = [[x,y] | x <- "abcdefgh", y <- "12345678"]

initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

filterNewLines string = Prelude.filter (/='\n') string 

positions = zip indicies (filterNewLines initialBoardStr)
 



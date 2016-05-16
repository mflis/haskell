board = [(x,y)| x <- [1..8], y <- [1..8]]

--hetmans::  (Enum a, Num a, Ord a) => [(a, a)] -> [(a, a)] -> Int -> [(a,a)]
hetmans :: (Enum a,Eq a, Num a,Ord a) => [(a,a)] -> [(a,a)] -> a -> [(a,a)] 

hetmans aviablePoints listOfHetmans 0 = listOfHetmans

hetmans aviablePoints listOfHetmans howManyLeft =
 hetmans points (newHetman:listOfHetmans) (howManyLeft -1)
 where
 newHetman = head aviablePoints
 points = filter (isAviable newHetman) aviablePoints 



isAviable:: (Enum a, Num a, Ord a) => (a, a) -> (a, a) -> Bool

isAviable  newHetman@(xh,yh) point@(xp,yp)
 | xp == xh = False
 | yp == yh = False
 | elem point (diagonals point) = False
 | otherwise = True



diagonals :: (Enum a, Num a, Ord a) => (a, a) -> [(a, a)]
diagonals (xh,yh) =
 filter isLegalPosition possibleDiags
 where possibleDiags = [(xh + increment, yh + increment) |  increment <-  [-8..8]] ++   [(xh - increment, yh + increment) |  increment <-  [-8..8]]   


isLegalPosition (x,y) =
 x > 0 && x < 9 &&  y > 0 && y < 9 
 
 

--,  x elem [1..8] , y elem [1..8]
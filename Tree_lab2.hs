data Tree a = EmptyTree | Node a (Tree a) (Tree a) 
 --deriving (Show, Read, Eq)  
-------------------------------------------------------------------------
 
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
---------------------------------------------------------------      
treeInsert :: (Ord a) => a -> Tree a -> Tree a  

treeInsert x EmptyTree = singleton x  

treeInsert x (Node a left right)   
        | x == a = Node x left right  
        | x < a  = Node a (treeInsert x left) right  
        | x > a  = Node a left (treeInsert x right)  

------------------------------------------------------------------
isEmpty :: Tree a -> Bool

isEmpty EmptyTree = True

isEmpty _ = False

--------------------------------------------------------------
search ::  (Ord a) =>  a-> (Tree a)  -> Bool

search  _ EmptyTree  = False
search val (Node a left right)
    | val == a = True  
    | val < a  = search val left  
    | val > a  = search val right  

---------------------------------------------------------------
testTree =  Node  100 (singleton 78) (singleton 200) 

testEmptyTree =  EmptyTree
testUnbalancedTree1 = Node 25 (Node 10  EmptyTree (singleton 12)) EmptyTree
testUnbalanced1 = Node 50  testUnbalancedTree1  EmptyTree 
testUnbalanced2 = Node 200  EmptyTree (singleton 500)


testUnbalanced = Node 100 testUnbalanced1 testUnbalanced2

testBalanced = Node 100 (singleton 20) (singleton 152)

-----------------------------------------------------------
isBinary ::  (Ord a) => Tree a -> Bool

isBinary  EmptyTree = True
isBinary (Node x EmptyTree EmptyTree  ) = True

isBinary (Node val EmptyTree right@(Node valRight _ _)) 
 = isBinary right && valRight > val 

isBinary (Node val left@(Node valLeft _ _) EmptyTree ) 
 = isBinary left  &&  valLeft <= val
     
isBinary (Node val left@(Node valLeft _ _)  right@(Node valRight _ _)) 
 = isBinary left &&
 isBinary right &&
 valLeft <= val &&
 valRight > val 

----------------------------------------------

isBalanced ::  (Ord a) => Tree a -> Bool

isBalanced (Node _ left right)
 = abs(getHeightOfTree left - getHeightOfTree right) <= 1 
------------------------------------------------

getHeightOfTree :: (Ord a) => Tree a -> Int

getHeightOfTree EmptyTree = 0

getHeightOfTree (Node _ left right)
 = max (getHeightOfTree left) (getHeightOfTree right)  + 1

-----------------------------------------------------------------------

traverseVLR :: (Ord a) => Tree a -> [a]

traverseVLR EmptyTree = []

traverseVLR (Node val left right) 
 = [val] ++ (traverseVLR left) ++ (traverseVLR right)
-----------------------------------------------------------------


toString EmptyTree = " "

toString (Node val EmptyTree EmptyTree) = show val

toString (Node val left right) = 
 show $ val ++ "(" ++ toString left ++ "," ++ toString right ++ ")"

---------------------------------------------------------

leaves EmptyTree = []

leaves (Node val EmptyTree EmptyTree) = [val]

leaves (Node val left right) = leaves left ++ leaves right


---------------------------------------------------------------------

nsum EmptyTree = 0

nsum (Node val EmptyTree EmptyTree) = val

nsum (Node val left right) = val +  nsum left + nsum right






--------------------------------------------------------
tmap :: (a -> a) -> Tree a -> Tree a

tmap _ EmptyTree = EmptyTree

tmap function (Node val left right) =
 Node (function val) (tmap function left) (tmap function right)

-----------------------------------------------------------------------------
maxInTree (Node val _ EmptyTree) = val
maxInTree (Node val _ right) = maxInTree right

searchSubtreeWithRoot x tree@(Node val left right) 
 | x == val = tree
 | x <  val = searchSubtreeWithRoot x left
 | x >  val = searchSubtreeWithRoot x right

treeRemove :: (Ord a) => a -> Tree a -> Tree a  

treeRemoveRoot  EmptyTree = error "Empty subtree" 

treeRemoveRoot  (Node x left EmptyTree ) = left

treeRemoveRoot  (Node x EmptyTree  right) = right

treeRemoveRoot  (Node x left right ) =
 Node newSubRoot leftWithoutMax right
 where
  newSubRoot = maxInTree left
  leftWithoutMax = treeRemove newSubRoot left


treeRemove x tree@(Node val left right)    
 | x == val = treeRemoveRoot (searchSubtreeWithRoot x tree)
 | x <  val = Node val (treeRemove x left) right
 | x >  val = Node val left (treeRemove x right)


 --------------------------------------------------------------------------
getLevel EmptyTree _ = []

getLevel (Node val left right)  1  = [val]  

getLevel (Node val left right)  level = 
 getLevel left (level -1) ++ getLevel right (level-1)

 ---------------------------------------------------------------------
makeLayoutFinal tree  = 
 foldl ( \acc x -> (length acc + 1, snd x, fst x):acc) [] (makeLayout tree 1) 

makeLayout EmptyTree _ = []

makeLayout (Node val left right) depth = 
 makeLayout left (depth +1) ++ [(val,depth)] ++ makeLayout right (depth +1)


-------------------------------------------------------------------------
getSubroot EmptyTree = ""

getSubroot (Node val _ _ ) = show val

dumpNODE EmptyTree = ""

dumpNODE tree@(Node val left right) =
 getSubroot tree  ++ " -> " ++ getSubroot left ++ "\n" 
 ++  getSubroot tree  ++ " -> " ++ getSubroot right ++ "\n" 
 ++ dumpNODE left 
 ++ dumpNODE right

 ----------------------------------------------------------------------



instance Show a => Show (Tree a)  where
 show EmptyTree = " - "
 show (Node val left right) = 
  "( " ++ (show left) ++ " " ++ (show val) ++ " " ++  (show right) ++ " )"

-----------------------------------------------------------------------------

data  STree a = SEmpty | SLeaf a | SBranch a (STree a) (STree a)
 deriving Show

convertStree EmptyTree = SEmpty 
convertStree (Node val EmptyTree EmptyTree) = SLeaf val
convertStree (Node val left right) = SBranch val (convertStree left) (convertStree right)







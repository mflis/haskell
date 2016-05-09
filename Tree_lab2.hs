data Tree a = EmptyTree | Node a (Tree a) (Tree a) 
 deriving (Show, Read, Eq)  

 
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
      
treeInsert :: (Ord a) => a -> Tree a -> Tree a  

treeInsert x EmptyTree = singleton x  

treeInsert x (Node a left right)   
        | x == a = Node x left right  
        | x < a  = Node a (treeInsert x left) right  
        | x > a  = Node a left (treeInsert x right)  


isEmpty :: Tree a -> Bool

isEmpty EmptyTree = True

isEmpty _ = False


search ::  (Ord a) =>  a-> (Tree a)  -> Bool

search  _ EmptyTree  = False

search val (Node a left right)
    | val == a = True  
    | val < a  = search val left  
    | val > a  = search val right  


testTree =  Node  100 (singleton 78) (singleton 200) 

testEmptyTree =  EmptyTree
testUnbalanced0 = Node 25 (Node 10  EmptyTree (singleton 12)) EmptyTree
testUnbalanced1 = Node 50  testUnbalanced0  EmptyTree 
testUnbalanced2 = Node 200  EmptyTree (singleton 500)


testUnbalanced = Node 100 testUnbalanced1 testUnbalanced2


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
          





sum' list  = foldl (\acc x -> acc + x) 0 list


product' list =  foldl (*) 1 list

rev' list = foldl (\acc x -> x:acc ) [] list

and' list = foldl1 (&&)  list

head' list = foldl1 ( \acc x-> acc ) list 

last' list = foldr1 ( \x acc -> acc ) list 
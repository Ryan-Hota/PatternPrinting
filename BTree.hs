module BTree where

--____________________________BTree_______________________________________________________________________

data BTree a = Nil | Node (BTree a) a (BTree a)
    deriving Eq 

sz :: BTree a -> Int 
sz Nil              = 0 
sz (Node tl x tr)   = sz tl + sz tr + 1

ht :: BTree a -> Int 
ht Nil              = 0 
ht (Node tl x tr)   = 1 + max (ht tl) (ht tr)

reflect :: BTree a -> BTree a
reflect Nil             = Nil 
reflect (Node tl x tr)  = Node (reflect tr) x (reflect tl)

levels :: BTree a -> [a]
levels  = concat . levels' where 
    levels' :: BTree a -> [[a]]
    levels' Nil             = []
    levels' (Node tl x tr)  = [x]:join (levels' tl) (levels' tr)
join :: [[a]] -> [[a]] -> [[a]]
join []         yss         = yss 
join xss        []          = xss 
join (xs:xss)   (ys:yss)    = (xs ++ ys): join xss yss  

inorder0 :: BTree a -> [a]
inorder0 Nil            = []
inorder0 (Node tl x tr) = inorder0 tl ++ [x] ++ inorder0 tr 

inorder :: BTree a -> [a]
inorder t           = go t [] where 
    go Nil              l   = l 
    go (Node tl x tr)   l   = go tl (x:go tr l)

ct0 :: [a] -> BTree a 
ct0 []  = Nil 
ct0 xs  = Node (ct0 f) x (ct0 b) where 
    n       = length xs 
    (f,x:b) = splitAt (n `div` 2) xs 

ct :: [a] -> BTree a 
ct xs   = fst (go (length xs) xs) where 
    go :: Int -> [a] -> (BTree a, [a])
--  go n xs = (ct0 (take n xs), drop n xs)
--  we have a guarantee that n <= length xs 
    go 0 xs     = (Nil, xs)
    go n xs     = (Node tl y tr, zs) where 
        m           = n `div` 2 
        (tl,y:ys)   = go m xs 
        (tr,zs)     = go (n-m-1) ys 

skewTree :: [a] -> BTree a 
skewTree    = foldr (Node Nil) Nil 

--_____________________________Ryan's_______________________________________________________________________

showNodes :: Show a => BTree a -> BTree String
showNodes Nil = Nil
showNodes (Node tl x tr) = Node (showNodes tl) (show x) (showNodes tr)

--______________________________Fin_______________________________________________________________________
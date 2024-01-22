module Blocky where

import BTree
import Image
import Colour

--___________________________print BTree_______________________________________________________________________

show' :: Show a => BTree a -> IO ()
show' = printImage . Wrap . drop 2 . unWrap . toImage . colourTreeRgb . showNodes

toImage :: BTree String -> Image
toImage Nil = Wrap [[Nothing]]
toImage (Node Nil x Nil) = wrap ["+","|"] * wrap (lines x)
toImage (Node tl  x tr)  = step (wrap $ lines x) (toImage tl) (toImage tr)

step :: Image -> Image -> Image -> Image
step rootImage leftImage rightImage = (*) (wrap ["+","|"]) $ (*) rootImage $ (*) (wrap ["|"]) $ miscDetails img where
    img = leftImage + Wrap [[Nothing]] + rightImage

miscDetails :: Image -> Image --miscellaneous details, remove and everything still essentially works
miscDetails img  = Wrap [addDash $ addPlus $ topLayer img] * Wrap ( tail ( unWrap img ) ) where
    addPlus l = zipWith (\x y-> if x==Just "+" then x else y) row1 row2 where--add a middle '+' 
                [row1, row2] = unWrap $ wrap ["+"] * Wrap [l]
    addDash l = l1 ++ l2 ++ l3 where  --add '-'s
                (l1, l3) = (takeWhile (==Nothing) l, takeWhile (==Nothing) (reverse l))
                l2 = map (\x-> if x==Nothing then Just "-" else x) $ (dropWhile (==Nothing).reverse.dropWhile (==Nothing).reverse) l

--________________________________Fin__________________________________________________________________

testBlocky :: Image
testBlocky = toImage $ colourTreeRgb $ ct $ replicate 31 "uWu\n(^-^)"
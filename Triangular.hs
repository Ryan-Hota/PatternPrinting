module Triangular where

import BTree
import Image
import Colour

--___________________________print BTree_______________________________________________________________________

instance Show a => Show (BTree a) where
    show =  show . toImage . colourTreeRainbow . showNodes 

toImage :: BTree String -> Image
toImage Nil = wrap ["*"]
toImage (Node Nil x Nil) = wrap $ lines x
toImage (Node tl x tr) = step (wrap $ lines x) 95 (toImage tl) 96 (toImage tr)

step :: Image -> Int -> Image -> Int -> Image -> Image
step rooti lc li rc ri  = negate $ ( lpad + hat + rpad ) * img
                        where
                            img = li + Wrap [middleStrip] + ri where
                                middleStrip = if even $ fromEnum top then [Nothing, Nothing] else [Nothing]
                            top = spread (li+ri) - spread lpad - spread rpad
                            hat = rooti * cap lc rc ((if even top then 2 else 1) + fromEnum top)
                            lpad = Wrap [ map (const Nothing) $ dropWhile (==Nothing) $ reverse (topLayer li) ]
                            rpad = Wrap [ map (const Nothing) $ dropWhile (==Nothing)           (topLayer ri) ]

cap :: Int -> Int -> ( Int -> Image )
cap leftColour rightColour n = leftLine + rightLine where
            m = div n 2
            leftLine  = negate $ Wrap $ take m $ map          (\x->Just (colourize leftColour  "/" ) : replicate x Nothing)  [1,3..]
            rightLine = negate $ Wrap $ take m $ map (reverse.(\x->Just (colourize rightColour "\\") : replicate x Nothing)) [1,3..]

--________________________________Fin__________________________________________________________________

testTriangular :: IO ()
testTriangular = printImage $ toImage $ colourTreeRainbow $ showNodes $ Node (ct [1..31]) 64 (Node Nil 1 Nil)
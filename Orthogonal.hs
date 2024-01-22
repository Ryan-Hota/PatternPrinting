import BTree
import Image
import Colour

--___________________________print BTree_______________________________________________________________________

collapse :: Bool
collapse = False

instance Show a => Show (BTree a) where
    show =  show . toImage . colourTreeRgb . showNodes

leftCat :: Image -> Image -> Image
leftCat i1 i2 = negate $ (i1 + fromInteger (spread i2)) * (i2 + fromInteger (spread i1))

toImage :: BTree String -> Image
toImage Nil = wrap ["*"]
toImage (Node Nil x Nil) = wrap (lines x)
toImage (Node tl x tr)  = ( ( wrap (lines x) * down' ) + wrap [across] + toImage tr ) `leftCat` ( (if collapse then indent else 0) + toImage tl ) where
    [tli, tri] = map toImage [tl, tr]
    down' = wrap (take (length $ unWrap tri) down)
    indent = Wrap [replicate (div ((-1)+fromEnum (spread $ wrap (lines x))) 2) Nothing]

across :: String
across = concat $ concat $ replicate 3 [colourize 96 "-",colourize 95 "-"]
down :: [String]
down = colourize 96 "|" : colourize 95 "|" : down

--________________________________Fin__________________________________________________________________

testOrthogonal :: IO ()
testOrthogonal = printImage $ toImage $ colourTreeRgb $ ct $ replicate 31 "uWu\n(^-^)"
import BTree
import Image
import Colour

import qualified Triangular as Base

--_____________________________________upDown Decisions__________________________________________________________________

-- instance Show a => Show (BTree a) where
--     show = show . Main.toImage . colourTreeRainbow . showNodes

errStr :: String
errStr = "SizeError : object(s) too large to display"

toImage :: BTree String -> Image
toImage = toImage' (toInteger terminalWidth)

center :: Integer -> Image -> Image
center = align " _ "

toImage' :: Integer -> BTree String -> Image
toImage' width tree@Nil = if width>=spread img then img else error errStr where img = Base.toImage tree
toImage' width tree@(Node Nil x Nil) = if width>=spread img then img else error errStr where img = Base.toImage tree
toImage' width tree@(Node tl x tr)
    |spread tri > width - 2 = rightDown width tree
    |spread tli > width - 2 = leftDown width tree
    |spread tli + 1 + spread tri > width = rightDown width tree
    |otherwise = Base.toImage tree
    where
        [tli, tri] = map Base.toImage [tl, tr]

--_____________________________________upDown implementation__________________________________________________________________

bar :: Int -> Image -> Image --top image requirements
bar colour img = wrap (map (colourize colour) (["*"]++replicate (-1+length (unWrap img)) "|"++["*"]))

leftDown :: Integer -> BTree String -> Image --send tl down
leftDown width Nil = error "debug"
leftDown width (Node tl x tr) = alignCat "_  " "_  " topi bottomi where
                                topi = Base.step (wrap $ lines x) 95 (bar 95 img) 96 img where
                                    img = toImage' (width-2) tr
                                bottomi = leftHalfImage $ Base.step (Wrap [[Nothing]]) 96 (Wrap [reverse $ topLayer img]) 95 img where
                                    img = toImage' (width-2) tl

leftHalfImage :: Image -> Image
leftHalfImage = Wrap . drop 1 . map leftHalfLayer . unWrap where
    leftHalfLayer layer = drop (div (length layer) 2) layer

rightDown :: Integer -> BTree String -> Image --send tr down
rightDown width Nil = error "debug"
rightDown width (Node tl x tr) = alignCat "  _" "  _" topi bottomi where
                                topi = Base.step (wrap $ lines x) 95 img 96 (bar 96 img) where
                                    img = toImage' (width-2) tl
                                bottomi = rightHalfImage $ Base.step (Wrap [[Nothing]]) 96 img  95 (Wrap [reverse $ topLayer img]) where
                                    img = toImage' (width-2) tr

rightHalfImage :: Image -> Image
rightHalfImage = Wrap . drop 1 . map leftHalfLayer . unWrap where
    leftHalfLayer layer = take (div (length layer) 2) layer

--________________________________Fin__________________________________________________________________


test :: Image
test = Base.step (Wrap [[Nothing]]) 96 (Wrap [reverse $ topLayer img]) 95 img where
    img = toImage' 130 $ showNodes $ ct [1..10]
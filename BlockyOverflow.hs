import BTree
import Image
import Colour

import qualified Blocky as Base

--_____________________________________upDown Decisions__________________________________________________________________

instance Show a => Show (BTree a) where
    show = show . Main.toImage . colourTreeRainbow . showNodes

errStr :: String
errStr = "SizeError : object(s) too large to display"

toImage :: BTree String -> Image
toImage = Wrap . drop 2 . unWrap . toImage' (toInteger terminalWidth)

center :: Integer -> Image -> Image
center = align " _ "

toImage' :: Integer -> BTree String -> Image
toImage' width tree@Nil = if width>=spread img then center width img else error errStr where img = Base.toImage tree
toImage' width tree@(Node Nil x Nil) = if width>=spread img then center width img else error errStr where img = Base.toImage tree
toImage' width tree@(Node tl x tr)
    |spread tri > width - 2 = rightDown width tree
    |spread tli > width - 2 = leftDown width tree
    |spread tli + 1 + spread tri > width = leftDown width tree
    |otherwise = center width $ Base.toImage tree
    where
        [tli, tri] = map Base.toImage [tl, tr]

--_____________________________________upDown implementation__________________________________________________________________

bar :: Image -> Image --top image requirements
bar img = wrap ("+" : replicate (length (unWrap img)) "|" )

leftDown :: Integer -> BTree String -> Image --send tl down
leftDown width Nil = error "debug"
leftDown width (Node tl x tr) = topi * bottomi where
                                topi = Base.step (wrap $ lines x) (bar img) img where
                                 img = toImage' (width-2) tr
                                bottomi = leftHalfImage $ Base.step (Wrap [[Nothing]]) (Wrap [reverse $ topLayer img]) img where
                                    img = toImage' (width-1) tl

leftHalfImage :: Image -> Image
leftHalfImage = Wrap . drop 4 . map leftHalfLayer . unWrap where
    leftHalfLayer layer = drop (div (length layer) 2) layer

rightDown :: Integer -> BTree String -> Image --send tr down
rightDown width Nil = error "debug"
rightDown width (Node tl x tr) = topi * bottomi where
                                topi = Base.step (wrap $ lines x) img (bar img) where
                                 img = toImage' (width-2) tl
                                bottomi = rightHalfImage $ Base.step (Wrap [[Nothing]]) img (Wrap [reverse $ topLayer img]) where
                                    img = toImage' (width-1) tr

rightHalfImage :: Image -> Image
rightHalfImage = Wrap . drop 4 . map leftHalfLayer . unWrap where
    leftHalfLayer layer = take (1+div (length layer) 2) layer

--________________________________Fin__________________________________________________________________


test :: Integer -> Image
test n = Base.step (Wrap [[Nothing]]) img (Wrap [reverse $ topLayer img])where
    img = toImage' n (Node Nil "2" (Node Nil "3" Nil))
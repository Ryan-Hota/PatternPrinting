module Colour where

import Image
import BTree

colourTreeDefault :: BTree String -> BTree String
colourTreeDefault Nil = Nil
colourTreeDefault (Node tl x tr) = Node (colourTreeDefault tl) x (colourTreeDefault tr)

colourTreeRainbow :: BTree String -> BTree String
colourTreeRainbow = rainbowTree 0 where
    rainbow i = if i<7 then colourize ([35,34,94,32,93,91,31]!!i) else error "empty colour"
    rainbowTree _ Nil = Nil
    rainbowTree i (Node tl x tr) = Node (rainbowTree (mod (i+1) 7) tl) (rainbow i x) (rainbowTree (mod (i+1) 7) tr)

colourTreeRgb :: BTree String -> BTree String
colourTreeRgb = rgbTree 0 where
    rgb i = if i<3 then map colourize [91,92,94]!!i else error "empty colour"
    rgbTree _ Nil = Nil
    rgbTree i (Node tl x tr) = Node (rgbTree (mod (i+1) 3) tl) (rgb i x) (rgbTree (mod (i+2) 3) tr)
module Image where

--______________________________Layer____________________________________________________________________

type Pixel = Maybe String --empty pixels are Nothing, (displayVal, ColourVal)

blank :: String
blank = " "

showPixel :: Pixel -> String
showPixel Nothing  = blank
showPixel (Just c) = c

type Layer = [Pixel] --A layer in an image, with Nothing denoting a blank pixel

showLayer :: Layer -> String
showLayer = concatMap showPixel

topLayer :: Image -> Layer
topLayer = head . unWrap

--______________________________Image____________________________________________________________________

newtype Image = Wrap [Layer] --deriving Show --an image is a list of layers stacked on top of each other
wrap :: [String] -> Image --wrap [String] instead of Wrap [Layer]
wrap = abs . Wrap . map help where
    help [] = []
    help ('\ESC':'[':x:y:'m':z:'\ESC':'[':'0':'m':xs) = Just ['\ESC', '[', x, y, 'm', z, '\ESC', '[', '0', 'm'] : help xs
    help (x:xs) = Just [x] : help xs
unWrap :: Image -> [Layer]
unWrap (Wrap l) = l

spread :: Image -> Integer --horizontal spread of the image
spread (Wrap l) = toInteger $ maximum $ map length l

instance Num Image where --the pictorial algebra

    --put two images next to each other
    (+) i@(Wrap i1) ii@(Wrap i2) = Wrap $ genJoin i1 i2
                            where
                                spaces = Nothing : spaces
                                genJoin []     ys     = map (take (fromInteger $ spread i) spaces ++) ys
                                genJoin xs     []     = map (++ take (fromEnum $ spread ii) spaces) xs
                                genJoin (x:xs) (y:ys) = (x ++ y) : genJoin xs ys

    --put one image on top of another
    (*) (Wrap i1) (Wrap i2) = abs $ Wrap $ i1 ++ i2

    --"strip" the image of blank pixels to make the image rectangular
    negate ii = Wrap $ map (drop n . reverse . drop m . reverse) i
        where
            i = unWrap $ abs ii
            n = minimum $ map (length . takeWhile (==Nothing)          ) i
            m = minimum $ map (length . takeWhile (==Nothing) . reverse) i

    --make the image rectangular by adding padding to each layer
    abs ii@(Wrap i) = Wrap $ map pad i where
        n = fromEnum $ spread ii
        pad l = lpad ++ l ++ rpad where
            m = n - length l
            (lpad,rpad) = splitAt (div m 2) (replicate m Nothing)

    --placeholder
    signum = id

    --[Nothing]*n
    fromInteger n = Wrap [replicate (fromEnum n) Nothing]

align :: String -> Integer -> Image -> Image
align s width img = Wrap $ tail $ unWrap $ 
                    (*) ((if s!!0=='_' then indent else 0)+fromInteger width+(if s!!2=='_' then indent else 0)) img where
                                            indent = (fromInteger $ width - spread img) :: Image


alignCat :: String -> String -> Image -> Image -> Image
alignCat tops bottoms topi bottomi = align tops width topi * align bottoms width bottomi where
                                                width = maximum $ map spread [topi, bottomi]

instance Show Image where
    show = unlines . map showLayer . unWrap . chunked

--__________________________displayOverflow__________________________________________________________________

terminalWidth :: Int
terminalWidth = 131

box :: Image -> Image
box img@(Wrap list) =                (   red ["_"]    +   red [horizontal] +   red ["_"]    )                -- \  ___
                    * ( red ["/"]    +  wrap [" "]    + green [horizontal] +  wrap [" "]    + red ["\\"]   ) -- \ / _ \
                    * ( red vertical + green vertical +        img         + green vertical + red vertical ) -- \ ||i||
                    * ( red ["|"]    + green ["|"]    + green [horizontal] + green ["|"]    + red ["|"]    ) -- \ ||_||
                    * ( red ["\\"]   +   red ["_"]    +   red [horizontal] +   red ["_"]    + red ["/"]    ) -- \ \___/
                    where
                        [red, green] = map (\n->wrap . map (colourize n)) [91,92]
                        horizontal = replicate (fromEnum $ spread img) '_'
                        vertical = map (const "|") list

chunked :: Image -> Image
chunked (Wrap i) = if all (==[]) i then indent + sign
                                   else box (Wrap i1) * chunked (Wrap i2) where
    [i1, i2] = map (`map` t) [fst,snd]
    t = map (splitAt terminalWidth) i
    indent = fromInteger (toInteger (terminalWidth+4-fromEnum (spread sign)))

sign :: Image
sign = wrap (lines $ colourize 91 "-- Yours lovingly, Gourab")

--_______________________________colour___________________________________________________________________

printImage :: Image -> IO ()
printImage = doAll . map (putStrLn.showLayer) . unWrap . chunked where
    doAll [] = return ()
    doAll (x:xs) = do {_<-x; doAll xs;}

toBeColoured :: String
toBeColoured = "`1234567890-=qwertyuiopppp[]\\asdfghjkl;'zxcvbnm,. /~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"

colourize :: Int -> String -> String
colourize n = concatMap (\x->if x `elem` toBeColoured then "\ESC["++show n++"m"++[x]++"\ESC[0m" else [x])

--________________________________Fin__________________________________________________________________

testImage :: IO ()
testImage = do
    printImage $ wrap (replicate 20 $ colourize 44 (replicate 120 ' '))
    testImage
module MagicCube(ctest1) where
-- Put the things that need to be inported in ()

import Graphics.Gloss

import Data.List

ctest1 :: IO ()
ctest1 = display window background test1

window :: Display
window = InWindow "Game" (500, 500) (100, 100)

background :: Color
background = black

test1 = pictures $ scaleP 20 20 $ (drawPuzzle p1) p1
test2 = pictures $ scaleP 20 20 $ (drawPuzzle p1) $ r p1
test3 = pictures $ scaleP 20 20 $ (drawPuzzle p1) $ f.l.l.f.r $ p1

f p = rotateLayer p 0
u p = rotateLayer p 1
l p = rotateLayer p 3
r p = rotateLayer p 2

ctest2 :: IO ()
ctest2 = display window background test2

ctest3 :: IO()
ctest3 = display window background test3

-- test = pictures $ scaleP 20 20 $ (drawTF p1 0) ++ (translateP 7 (-7) (drawTF p1 1)) ++ (translateP 0 7 (drawTF p1 2)) ++ (translateP (-7) (-7) (drawTF p1 3))
-- test = scale 20 20 $ drawSticker s1
-- test = pictures $ scaleP 20 20 $ [drawSticker s1, drawSticker s2, drawSticker s3, drawSticker s4]
-- test = color orange $ rotate 90 $ translate 50 50 $ scale 20 40 $ polygon $ makeRSticker 4

type NVector = [Float]

type Layer = [Cubie]

type Face = [Picture]


data Sticker = Sticker {face :: [Float], vS :: [(Float,Float)], c :: Color, ns :: String} deriving (Eq)

instance Show Sticker where 
        show (Sticker x y z n) = show n ++ ":" ++ show x
        
drawSticker :: Sticker -> Picture
drawSticker s = color (c s) $ polygon $ vS s

makeRSticker :: Int -> [(Float,Float)]
makeRSticker n = let ones = 1.0 : ones
                     zero = 0.0 : zero
                     v = takeWhile (\(x,y,z) -> x <= n) $ zip3 [1..] zero ones
                     v' = map (\(r,x,y) -> 
                                         let rotation = (2*pi*fromIntegral r)/ fromIntegral n
                                             x' = x*cos(rotation) - y*sin(rotation)
                                             y' = y*cos(rotation) + x*sin(rotation)
                                         in (x',y')) v
                 in (v')
                 
onFace :: Sticker -> Int -> Bool
onFace s l = foldl (\x y -> x || y) False $ zipWith (\i e -> i==l) [0..] $ face s

cloneS :: Sticker -> Sticker
cloneS s = s

rotS :: Sticker -> Float -> Sticker
rotS s r = s {vS = map (\v -> toTuple $ transform [[cos r, -1*sin r],[sin r, cos r]] [fst v, snd v]) $ vS s}

translateS :: Sticker -> Float -> Float -> Sticker
translateS s x y = s {vS = map (\(xo, yo) -> (xo+x,yo+y)) (vS s)}

changeFace :: Sticker -> [Float] -> Sticker
changeFace s f = s {face = f}

changeColor :: Sticker -> Color -> Sticker
changeColor s nc = s {c = nc}

-- toCenterS :: Sticker -> Int -> Sticker
-- toCenterS s f = s {face = transpose $ rotateL f $ transpose $ face s}

        
data Cubie = Cubie {position :: [Float], stickers :: [Sticker], nc :: String} deriving (Eq)

instance Show Cubie where
        show (Cubie x y n) = show n ++ "\n Position: " ++ show x ++ (foldl1 (++) (zipWith (\s i -> "\n \t Sticker " ++ show i ++ ": " ++ show s) y [0..])) ++ "\n"

rotateCubie :: Cubie -> [[Float]] -> Cubie
rotateCubie c m = Cubie {position = transform m $ position c, stickers = map (\s -> s {face = transform m $ face s}) $ stickers c, nc = nc c}

inLayer :: Cubie -> Int -> Bool
inLayer c l = foldl (\x s -> x || (foldl (||) False $ zipWith (\i e -> e>0&&i==l) [0..] $ face s)) False $ stickers c
                
getSticker :: Cubie -> Int -> Sticker
getSticker c f = foldl1 (\x y -> x) $ filter (\s -> case elemIndex 1 (face s) of
                                                                Just y -> y == f
                                                                _ -> False) $ stickers c
                                                                
-- toCenterC :: Cubie -> Int -> Cubie
-- toCenterC c f = c {position = transpose $ rotateL f $ transpose $ position c, stickers = map (\s -> toCenterS s f) $ stickers c}


-- data Puzzle = Puzzle {cubies :: [Cubie], drawPuzzle :: (Puzzle -> [Picture]), drawFace :: (Puzzle -> Int -> [Picture])}
data Puzzle = Puzzle {cubies :: [Cubie], drawPuzzle :: (Puzzle -> [Picture])}

instance Show Puzzle where
        show (Puzzle x y) = foldl1 (++) (zipWith (\c i -> "Cubie " ++ show i ++ ": " ++ show c) x [0..])

drawT :: Puzzle -> [Picture]
drawT p = let fa = getAdjFaces p 0
              bs = rotateL ((length fa)-1) $ makeRSticker $ length fa
              fs = filter (\(f, i) -> f `elem` fa) $ (zip (getFaces p) [0..])
              cf = drawTF p 0
          in foldl (\x y -> x ++ y) cf $ zipWith (\(f, i) (x, y) -> case i of 
                                                                            1 -> translateP (x*7) (y*7) $ scaleP 1 (-1) $ drawTF p i
                                                                            2 -> translateP (x*7) (y*7) $ rotateP 60 $ drawTF p i
                                                                            3 -> translateP (x*7) (y*7) $ scaleP (-1) 1 $ rotateP 60 $ drawTF p i) fs bs
                                                                            
drawTF p f = let bs = rotateL ((length $ getAdjFaces p f)-1) $ makeRSticker $ length $ getAdjFaces p f
             in map (\q -> let ts = zipWith (\(x, y) (e, i) -> (x*e*2, y*e*2)) bs $ removeItem (1, f) $ zip (position q) [0..]
                               s = drawSticker $ getSticker q f
                           in foldl (\s' (x, y) -> translate x y s') s ts) $ getLayer p f

-- drawT :: Puzzle -> [Picture]
-- drawT p = let fa = getAdjFaces p 0
              -- bs = rotateL ((length fa)-1) $ makeRSticker $ length fa
              -- fs = filter (\(f, i) -> f `elem` fa) $ (zip (getFaces p) [0..])
              -- cf = drawTF p 0
              -- -- a = 360/(fromIntegral $ length $ getAdjFaces p 0)
          -- in foldl (\x y -> x ++ y) cf $ zipWith (\(f, i) (x, y) -> case i of 
                                                                            -- 1 -> translateP (x*7) (y*7) $ scaleP 1 (-1) $ (drawFace p) p i
                                                                            -- 2 -> translateP (x*7) (y*7) $ rotateP 60 $ (drawFace p) p i
                                                                            -- 3 -> translateP (x*7) (y*7) $ scaleP (-1) 1 $ rotateP 60 $ (drawFace p) p i) fs bs
          -- -- in foldl (\x y -> x ++ y) cf $ zipWith (\(f, i) (x, y) -> translateP (x*7) (y*7) $ (drawFace p) p i) fs bs
          -- -- in foldl (\x y -> x ++ y) cf $ zipWith (\(f, i) r -> rotateP (-1*r*a) $ translateP 0 5 $ scaleP 1 (-1) $ rotateP (r*a) $ (drawTF p i)) fs [0..]
-- -- rotateP (-1*r*a) $ 
-- drawTF p f = let pt = toCenter p f
                 -- -- bs = reverse $ makeRSticker $ length $ getAdjFaces pt f
                   -- -- bs0 = bs !! ((length bs)-1)
                   -- -- bs' = [bs0] ++ (removeItem bs0 bs)
               -- in map (\q -> let ts = zipWith (\(x, y) (e, i) -> (x*e*2, y*e*2)) bs $ removeItem (1, 0) $ zip (position q) [0..]
                                 -- s = drawSticker $ getSticker q 0
                             -- in foldl (\s' (x, y) -> translate x y s') s ts) $ getLayer pt 0
                             
-- drawTF p f = let bs = rotateL ((length $ getAdjFaces p f)-1) $ makeRSticker $ length $ getAdjFaces p f
                   -- -- bs0 = bs !! ((length bs)-1)
                   -- -- bs' = [bs0] ++ (removeItem bs0 bs)
             -- in map (\q -> let ts = zipWith (\(x, y) (e, i) -> (x*e*2, y*e*2)) bs $ removeItem (1, f) $ zip (position q) [0..]
                               -- s = drawSticker $ getSticker q f
                           -- in foldl (\s' (x, y) -> translate x y s') s ts) $ getLayer p f

--drawTF p f = map (\q -> color (c $ getSticker q f) $ polygon $ map (\v -> toPlane (position q) v) $ vS (getSticker q f)) $ getLayer p f
-- drawTF p f = map (\q -> let s = drawSticker $ getSticker q f
                         -- in case f of
                                    -- 0 -> rotate    0   $ translate 0 0 $ scale 1   1  $ rotate   0    s
                                    -- 1 -> rotate    0   $ translate 0 5 $ scale 1 (-1) $ rotate   0    s
                                    -- 2 -> rotate (-120) $ translate 0 5 $ scale 1 (-1) $ rotate   120  s
                                    -- 3 -> rotate   120  $ translate 0 5 $ scale 1 (-1) $ rotate (-120) s
                                    -- _ -> error ("error")) $ getLayer p f

-- toPlane :: [Float] -> (Float,Float)
-- toPlane v = let x = 5 * (v !! 1) * (-1 * v !!2)
                -- y = -5*(v!!3)
            -- in (x,y)
            
-- toCenter :: Puzzle -> Int -> Puzzle
-- toCenter p f = let af = filter (\f -> ) $ getAdjFaces p f
               -- in p {cubies = map (\c -> rotateCubie c r) $ cubies p}

getLayer :: Puzzle -> Int -> Layer
getLayer p l = filter (\c -> inLayer c l) $ cubies p   

rotateLayer :: Puzzle -> Int -> Puzzle
rotateLayer p f = let cl = getLayer p f
                      r = getRMatrix p f
                      cr = map (\c -> rotateCubie c r) $ cl
                      np = filter (\c -> not (c `elem` cl)) $ cubies p
                  in p {cubies = cr ++ np}
                  
repRotate :: Puzzle -> Int -> Int -> Puzzle
repRotate p f n = case n of 
                       1 -> rotateLayer p f
                       _ -> repRotate (rotateLayer p f) f (n-1)

getRMatrix :: Puzzle -> Int -> [[Float]]
getRMatrix p f = let fs = getAdjFaces p f
                     fr = fs !! 0
                     fs' = (removeItem fr fs)
                     fr' = fs' ++ [fr]
                     ex = filter (\(e, i) -> not $ e `elem` fs) $ zipWith (\i e -> (e, i)) [0..] $ getFaces p
                     n = merge fr' ex
                 in n
                 
rotateLayer' :: Puzzle -> Int -> Puzzle
rotateLayer' p f = let cl = getLayer p f
                       r = getRMatrix' p f
                       cr = map (\c -> rotateCubie c r) $ cl
                       np = filter (\c -> not (c `elem` cl)) $ cubies p
                   in p {cubies = cr ++ np}
                  
getRMatrix' :: Puzzle -> Int -> [[Float]]
getRMatrix' p f = let fs = getAdjFaces p f
                      fr = fs !! ((length fs)-1)
                      fs' = (removeItem fr fs)
                      fr' = [fr] ++ fs'
                      ex = filter (\(e, i) -> not $ e `elem` fs) $ zipWith (\i e -> (e, i)) [0..] $ getFaces p
                      n = merge fr' ex
                   in n
                                                                                                
getAdjFaces :: Puzzle -> Int -> [[Float]]
getAdjFaces p f = reverse $ sort $ remDups $ filter (\x -> length x > 1) $ foldl1 (\x y -> x ++ y) $ map (\c -> case (length $ stickers c) /= 1 of
                                                                                                                        True -> map (\x -> face x) $ (filter (\s -> not $ foldl1 (\x y -> x||y) (zipWith (\i e -> e > 0 && i == f) [0..] $ face s))) $ stickers c
                                                                                                                        False -> [[fromIntegral f]]) $ getLayer p f


getFaces :: Puzzle -> [[Float]]
getFaces p = reverse $ sort $ remDups $ foldl1 (\x y -> x ++ y) $ map (\c -> map (\s -> face s) $ stickers c) $ cubies p


rotateL :: Int -> [a] -> [a]
rotateL _ [] = []
rotateL n xs = zipWith const (drop n (cycle xs)) xs

merge :: [[Float]] -> [([Float],Int)] -> [[Float]]
merge x [] = x
merge x ((y,i):ys) = merge ((fst $ splitAt i x) ++ [y] ++ (snd $ splitAt i x)) ys
                
scaleP :: Float -> Float -> [Picture] -> [Picture]
scaleP x y s = map (\p -> scale x y p) s

translateP :: Float -> Float -> [Picture] -> [Picture]
translateP x y s = map (\p -> translate x y p) s

rotateP :: Float -> [Picture] -> [Picture]
rotateP d s = map (\p -> rotate d p) s
                
remDups = foldl (\seen x -> if x `elem` seen
                            then seen
                            else seen ++ [x]) []
                            
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
                
transform :: [[Float]] -> [Float] -> [Float]
transform m v = map (\r -> cross r v) m

cross :: [Float] -> [Float] -> Float
cross v1 v2 = foldl1 (+) $ zipWith (\x y -> (x)*(y)) v1 v2

dToR :: Float -> Float
dToR d = d/57.2958

toTuple :: [Float] -> (Float, Float)
toTuple (x:y:_) = (x,y)

-- skew :: [(Float, Float)] -> Float -> [(Float, Float)]
-- skew vs s = map (\(x,y) -> case (-0.005)<x && x<0.005 of 
                                -- True -> (x,y)
                                -- _ -> case x<0.005 of
                                          -- False -> (x,y*(s*x))
                                          -- True -> (x,y*(1/((s*x))))) vs
                                          

s0, s1, s2, s3 :: Sticker
s0 = Sticker {face = [1,0,0,0], vS = makeRSticker 3, c = red, ns = "s1"}
s1 = Sticker {face = [0,1,0,0], vS = makeRSticker 3, c = blue, ns = "s2"}
s2 = Sticker {face = [0,0,1,0], vS = makeRSticker 3, c = green, ns = "s3"}
s3 = Sticker {face = [0,0,0,1], vS = makeRSticker 3, c = orange, ns = "s4"}


cc0, cc1, cc2, cc3, cc :: Cubie
cc0 = Cubie {position = [1,1,1,0], stickers = [rotS s0 pi, rotS s1 pi, rotS s2 pi], nc = "cc0"}
cc1 = Cubie {position = [1,1,0,1], stickers = [rotS s0 pi, rotS s1 pi, rotS s3 pi], nc = "cc1"}
cc2 = Cubie {position = [1,0,1,1], stickers = [rotS s0 pi, rotS s2 pi, rotS s3 pi], nc = "cc2"}
cc3 = Cubie {position = [0,1,1,1], stickers = [rotS s1 pi, rotS s2 pi, rotS s3 pi], nc = "cc3"}
cc  = Cubie {position = [1,1,1,1], stickers = [cloneS s0, cloneS s1, cloneS s2, cloneS s3], nc = "cc"}

p1 :: Puzzle
p1 = Puzzle {cubies = [cc0,cc1,cc2,cc3,cc], drawPuzzle = drawT}

-- p1r :: Puzzle
-- p1r = rotateLayer p1 0

-- ps :: Puzzle
-- ps = Puzzle {cubies = [cc], drawPuzzle = drawT, drawFace = drawTF}


-- sc, s1, s2, s3 :: Sticker
-- sc = Sticker {face = [1,0,0,0], vS = makeRSticker 3, c = red, ns = "s1"}
-- s1 = translateS (rotS sc (pi/3)) 1.7 1.0
-- s2 = translateS (rotS sc (pi/3)) (-1.7) 1.0
-- s3 = translateS (rotS sc (pi)) 0 (-2) 


-- cc0 :: Cubie
-- cc0 = Cubie {position = [1,1,1,0], stickers = [cloneS s2, changeFace (changeColor s3 blue) [0,1,0,0], changeFace (changeColor s1 green) [0,0,1,0]], nc = "cc0"}
-- cc1 = Cubie {position = [1,1,1,0], stickers = [cloneS s3, changeFace (changeColor s2 blue) [0,1,0,0], changeFace (changeColor s3 green) [0,0,1,0]], nc = "cc1"}
-- cc2 = Cubie {position = [1,1,0,1], stickers = [cloneS s2, changeFace (changeColor s3 blue) [0,1,0,0], changeFace (changeColor s2 orange) [0,0,0,1]], nc = "cc2"}
-- cc3 = Cubie {position = [1,0,1,1], stickers = [cloneS s4, changeFace (changeColor s4 green) [0,0,1,0], changeFace (changeColor s4 orange) [0,0,0,1]], nc = "cc3"}
-- cc4 = Cubie {position = [0,1,1,1], stickers = [changeFace (changeColor s4 blue) [0,1,0,0], changeFace (changeColor s2 green) [0,0,1,0], changeFace (changeColor s3 orange) [0,0,0,1]], nc = "cc4"}
-- cc1 = Cubie {position = [1,1,1,1], stickers = [s1,s2,s3,s4], nc = "test"}

-- cc :: Cubie
-- cc = Cubie {position = [1,1,1,1], stickers = [cloneS sc, changeFace (changeColor sc blue) [0,1,0,0], changeFace (changeColor sc green) [0,0,1,0], changeFace (changeColor sc orange) [0,0,0,1]], nc = "cc"}

-- p1 :: Puzzle
-- -- p1 = Puzzle {cubies = [cc1, cc2, cc3, cc4, cc]}
-- p1 = Puzzle {cubies = [cc0, cc]}

-- p1r :: Puzzle
-- p1r = rotateLayer p1 0
module Main(main) where

import Graphics.Gloss
import Data.List

main :: IO ()
main = display window background test

window :: Display
window = InWindow "Game" (500, 500) (100, 100)

background :: Color
background = black

test = pictures $ scaleP 20 20 $ drawPuzzle p1

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

rotS :: Float -> Sticker -> Sticker
rotS r s = s {vS = map (\v -> toTuple $ transform [[cos r, -1*sin r],[sin r, cos r]] [fst v, snd v]) $ vS s}

translateS :: Float -> Float -> Sticker -> Sticker
translateS x y s = s {vS = map (\(xo, yo) -> (xo+x,yo+y)) (vS s)}

changeFace :: Sticker -> [Float] -> Sticker
changeFace s f = s {face = f}

changeColor :: Sticker -> Color -> Sticker
changeColor s nc = s {c = nc}

        
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


data Puzzle = Puzzle {cubies :: [Cubie]} deriving (Eq)

instance Show Puzzle where
        show (Puzzle x) = foldl1 (++) (zipWith (\c i -> "Cubie " ++ show i ++ ": " ++ show c) x [0..])

drawPuzzle :: Puzzle -> [Picture]
drawPuzzle p = foldl1 (\x y -> x ++ y) $ map (\(i, f) -> getFace p i) $ zip [0..] $ getFaces p

getFace :: Puzzle -> Int -> Face
--getFace p f = map (\q -> color (c $ getSticker q f) $ polygon $ map (\v -> toPlane (position q) v) $ vS (getSticker q f)) $ getLayer p f
getFace p f = map (\q -> let s = drawSticker $ getSticker q f
                         in case f of
                                    0 -> rotate     0 $ translate 0  0   s
                                    1 -> rotate   180 $ translate 0 (-5) s
                                    2 -> rotate    60 $ translate 0 (-5) s
                                    3 -> rotate (-60) $ translate 0 (-5) s
                                    _ -> error ("error")) $ getLayer p f

-- toPlane :: [Float] -> (Float,Float)
-- toPlane v = let x = 5 * (v !! 1) * (-1 * v !!2)
                -- y = -5*(v!!3)
            -- in (x,y)

getLayer :: Puzzle -> Int -> Layer
getLayer p l = filter (\c -> inLayer c l) $ cubies p   

rotateLayer :: Puzzle -> Int -> Puzzle
rotateLayer p f = let cl = getLayer p f
                      r = getRMatrix p f
                      cr = map (\c -> rotateCubie c r) $ cl
                      np = filter (\c -> not (c `elem` cl)) $ cubies p
                  in Puzzle {cubies = cr ++ np}

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
                   in Puzzle {cubies = cr ++ np}
                  
getRMatrix' :: Puzzle -> Int -> [[Float]]
getRMatrix' p f = let fs = getAdjFaces p f
                      fr = fs !! ((length fs)-1)
                      fs' = (removeItem fr fs)
                      fr' = fs' ++ [fr]
                      ex = filter (\(e, i) -> not $ e `elem` fs) $ zipWith (\i e -> (e, i)) [0..] $ getFaces p
                      n = merge fr' ex
                  in n
                                                                                                
getAdjFaces :: Puzzle -> Int -> [[Float]]
getAdjFaces p f = reverse $ sort $ remDups $ filter (\x -> length x > 1) $ foldl1 (\x y -> x ++ y) $ map (\c -> case (length $ stickers c) /= 1 of
                                                                                                                        True -> map (\x -> face x) $ (filter (\s -> not $ foldl1 (\x y -> x||y) (zipWith (\i e -> e > 0 && i == f) [0..] $ face s))) $ stickers c
                                                                                                                        False -> [[fromIntegral f]]) $ getLayer p f

merge :: [[Float]] -> [([Float],Int)] -> [[Float]]
merge x [] = x
merge x ((y,i):ys) = merge ((fst $ splitAt i x) ++ [y] ++ (snd $ splitAt i x)) ys
                                                                                                                        
getFaces :: Puzzle -> [[Float]]
getFaces p = reverse $ sort $ remDups $ foldl1 (\x y -> x ++ y) $ map (\c -> map (\s -> face s) $ stickers c) $ cubies p
                
scaleP :: Float -> Float -> [Picture] -> [Picture]
scaleP x y s = map (\p -> scale x y p) s
                
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

skew :: [(Float, Float)] -> Float -> [(Float, Float)]
skew vs s = map (\(x,y) -> case (-0.005)<x && x<0.005 of 
                                True -> (x,y)
                                _ -> case x<0.005 of
                                          False -> (x,y*(s*x))
                                          True -> (x,y*(1/((s*x))))) vs
                                          

s1, s2, s3, s4 :: Sticker
s1 = Sticker {face = [1,0,0,0], vS = makeRSticker 3, c = red, ns = "s1"}
s2 = translateS 1.7 1.0 $ rotS (pi/3) s1
s3 = translateS (-1.7) 1.0 $ rotS (pi/3) s1
s4 = translateS 0 (-2) $ rotS (pi) s1

cc1, cc2, cc3, cc4 :: Cubie
cc1 = Cubie {position = [1,1,1,0], stickers = [cloneS s3, changeFace (changeColor s2 blue) [0,1,0,0], changeFace (changeColor s3 green) [0,0,1,0]], nc = "cc1"}
cc2 = Cubie {position = [1,1,0,1], stickers = [cloneS s2, changeFace (changeColor s3 blue) [0,1,0,0], changeFace (changeColor s2 orange) [0,0,0,1]], nc = "cc2"}
cc3 = Cubie {position = [1,0,1,1], stickers = [cloneS s4, changeFace (changeColor s2 green) [0,0,1,0], changeFace (changeColor s3 orange) [0,0,0,1]], nc = "cc3"}
cc4 = Cubie {position = [0,1,1,1], stickers = [changeFace (changeColor s4 blue) [0,1,0,0], changeFace (changeColor s4 green) [0,0,1,0], changeFace (changeColor s4 orange) [0,0,0,1]], nc = "cc4"}

cc:: Cubie
cc = Cubie {position = [1,1,1,1], stickers = [cloneS s1, changeFace (changeColor s1 blue) [0,1,0,0], changeFace (changeColor s1 green) [0,0,1,0], changeFace (changeColor s1 orange) [0,0,0,1]], nc = "cc"}

p1 :: Puzzle
p1 = Puzzle {cubies = [cc1, cc2, cc3, cc4, cc]}

p1r :: Puzzle
p1r = rotateLayer p1 0
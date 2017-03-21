module Sticker where

import Graphics.Gloss

transform :: [[Float]] -> [Float] -> [Float]
transform m v = map (\r -> cross r v) m

cross :: [Float] -> [Float] -> Float
cross v1 v2 = foldl1 (+) $ zipWith (\x y -> (x)*(y)) v1 v2

toTuple :: [Float] -> (Float, Float)
toTuple (x:y:_) = (x,y)

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

rotS :: Sticker -> Float -> Sticker
rotS s r = s {vS = map (\v -> toTuple $ transform [[cos r, -1*sin r],[sin r, cos r]] [fst v, snd v]) $ vS s}

translateS :: Sticker -> Float -> Float -> Sticker
translateS s x y = s {vS = map (\(xo, yo) -> (xo+x,yo+y)) (vS s)}

changeFace :: Sticker -> [Float] -> Sticker
changeFace s f = s {face = f}

changeColor :: Sticker -> Color -> Sticker
changeColor s nc = s {c = nc}

-- toCenterS :: Sticker -> Int -> Sticker
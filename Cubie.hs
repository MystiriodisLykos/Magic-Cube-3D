module Cubie where

import Graphics.Gloss
import Sticker
import Data.List

type Layer = [Cubie]
       
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
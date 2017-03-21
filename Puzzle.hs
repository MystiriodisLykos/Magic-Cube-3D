module Puzzle where

import Graphics.Gloss
import Data.List
import Sticker
import Cubie

remDups :: (Eq a, Foldable t) => t a -> [a]
remDups = foldl (\seen x -> if x `elem` seen
                            then seen
                            else seen ++ [x]) []
                            
merge :: [[Float]] -> [([Float],Int)] -> [[Float]]
merge x [] = x
merge x ((y,i):ys) = merge ((fst $ splitAt i x) ++ [y] ++ (snd $ splitAt i x)) ys

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

data Puzzle = Puzzle {cubies :: [Cubie], drawPuzzle :: (Puzzle -> [Picture])}

instance Show Puzzle where
        show (Puzzle x y) = foldl1 (++) (zipWith (\c i -> "Cubie " ++ show i ++ ": " ++ show c) x [0..])
            
-- toCenter :: Puzzle -> Int -> Puzzle

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
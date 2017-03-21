module MagicCube where

import Graphics.Gloss
import Sticker
import Cubie
import Puzzle
import Data.List

window :: Display
window = InWindow "Game" (500, 500) (100, 100)

background :: Color
background = black

f p = rotateLayer p 0
u p = rotateLayer p 1
l p = rotateLayer p 3
r p = rotateLayer p 2

test1 = pictures $ scaleP 20 20 $ (drawPuzzle p1) p1
test2 = pictures $ scaleP 20 20 $ (drawPuzzle p1) $ r p1
test3 = pictures $ scaleP 20 20 $ (drawPuzzle p1) $ f.l.l.f.r $ p1

ctest1 :: IO ()
ctest1 = display window background test1

ctest2 :: IO ()
ctest2 = display window background test2

ctest3 :: IO()
ctest3 = display window background test3
                                          
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

module Main where

import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime :: RealFrac a => Point -> Point -> a -> Picture
dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))


--Q5
pointAintercaler :: Point -> Point -> Point
pointAintercaler (xA, yA) (xB, yB) = ((xA + xB)/2 + (yB - yA)/2, (yA + yB)/2 + (xA - xB)/2)

--Q6
pasDragon :: Path -> Path
pasDragon [] = []
pasDragon [x] = [x]
pasDragon [a, b] = a : pointAintercaler a b : [b]
pasDragon (x : y : z : zs) = x : pointAintercaler x y : y : pointAintercaler z y : pasDragon (z:zs)

--Q7
dragon :: Point -> Point -> [Path]
dragon p1 p2 = iterate pasDragon [p1,p2]

--Q8
dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre p1 p2 0 = [p1, p2]
dragonOrdre p1 p2 n = dragonOrdre p1 p3 (n-1) ++ dragonOrdre p2 p3 (n-1) where p3 = pointAintercaler p1 p2


-- PARTIE 1 --

import Graphics.Gloss

sommeTest n = sum [1..n]

--Q3
sommeDeXaY :: Int -> Int -> Int
sommeDeXaY x y = sum [x..y]


--Q4
somme :: [Int] -> Int
somme [] = 0
somme (x:xs) = sum (x:xs)


--Q5
lastTest :: [a] -> a
lastTest [x] = x
lastTest (x:xs) = head (reverse xs)


initTest :: [a] -> [a]
initTest [x] = []
initTest (x:xs) = take ((length (x:xs))-1) (x:xs)


--Q6
--réécriture de !!
getValue :: Int -> [a] -> a
getValue n (x:xs) = head (drop n (x:xs)) 

--réécriture de ++
plusPlus :: [a] -> [a] -> [a]
plusPlus [] [] = []
plusPlus [] ys = ys  -- cas de récurrence
plusPlus (x:xs) ys = x : (plusPlus xs ys)

--réécriture de concat
concatCustom :: [[a]] -> [a]
concatCustom [] = []
concatCustom ([]:ys) = concatCustom ys   -- cas de récurrence
--concaténation de la tête de la première liste avec le résultat de la fonction 
concatCustom ((x:xs):ys) = x:concatCustom (xs:ys)


--réécriture de map
mapCustom :: (a -> b) -> [a] -> [b]
mapCustom f [] = [] -- cas de récurrence
mapCustom f [x] = (f x):[]
-- on applique la fonction f sur la tête de la liste et on concat avec la récurrence qui applique la fonction f sur le reste de la liste
mapCustom f (x:xs) = f x : mapCustom f xs


--Q7
-- pass


--Q8
longueur :: [Int] -> Int
longueur [] = 0
-- on applique puissance 0 sur tous les éléments pour les mettre à 1 puis on additionne la liste ce qui nous donne la taille
longueur (x:xs) = somme (map (^0) (x:xs))


--Q9 récursion
fonctionQ9 :: (a -> a) -> a -> Int -> [a]
fonctionQ9 f x 0 = [x]     -- cas de récurrence
-- on applique la fonction sur le nouvel élement qui est (f x)
fonctionQ9 f x n =  x : fonctionQ9 f (f x) (n-1) 


--Q9 iterate & take
fonctionQ9' :: (a -> a) -> a -> Int -> [a]
fonctionQ9' f x n = take (n+1) (iterate (f) x)


--Q10
generateIntList :: Int -> [Int]
-- On applique la fonction (+1) à chaque récursion de la fonction Q9
generateIntList n = fonctionQ9 (+1) 0 n


-- PARTIE 2 --


--Q1
alterne :: [a] -> [a]
alterne [] = []   -- cas de fin de récursion
--on ajoute la head de la liste puis on enlève 1 avec drop et la tête du reste ect
alterne (x:xs) = x : alterne (drop 1 xs)


--Q2
combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f [] [] = []
-- cas d'arret de la récursion
combine f [] (y:_) = []
combine f (x:_) [] = []
-- on fait l'opération f sur les deux head de la liste puis en récusrion sur le reste
combine f (x:xs) (y:ys) = f x y : combine f xs ys


--Q3
pasPascal :: [Integer] -> [Integer]
pasPascal [] = []
-- on ajoute zip la liste avec sa tail puis on ajoute 1 de chaque coté
pasPascal (x:xs) = 1 : (zipWith (+) (x:xs) xs) ++ [1]


--Q4
pascal :: [[Integer]]
pascal = iterate pasPascal [1]


-- Courbe du dragon

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


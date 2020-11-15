module TP2 where

import  Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

type EtatTortue = (Point, Float)

type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue


type EtatDessin = (EtatTortue, Path)

-- Question 1
motSuivant :: Regles -> Mot -> Mot
motSuivant r [] = []
motSuivant r (x:xs) = (r x) ++ (motSuivant r xs)

motSuivant' :: Regles -> Mot -> Mot
motSuivant' r m = concat [r x | x <- m]

motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' = concatMap -- concat (map r m)

-- Question 2
regleKoch :: Symbole -> Mot
regleKoch 'F' = "F-F++F-F"
regleKoch '+' = "+"
regleKoch '-' = "-"


-- Question 3
lsysteme :: Axiome -> Regles -> LSysteme
lsysteme m r = iterate (motSuivant r ) m

-- Tortue

-- Question 4
etatInitial :: Config -> EtatTortue
etatInitial (a,_,_,_,_) = a


longueurPas :: Config -> Float
longueurPas (_,a,_,_,_) = a


facteurEchelle :: Config -> Float
facteurEchelle (_,_,a,_,_) = a


angle :: Config -> Float
angle (_,_,_,a,_) = a


symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,a) = a


-- Question 5
avance :: Config -> EtatTortue -> EtatTortue
avance f ((x,y),t) = ((x + (longueurPas f) * cos t,y + (longueurPas f) * sin t),t)

-- Question 6
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche f (p,t) = (p,t + (angle f))


tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite f (p,t) = (p,t - (angle f))


-- Question 7
filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue _ [] = []
filtreSymbolesTortue f (m:m1) = 
    if elem m (symbolesTortue f) then m : (filtreSymbolesTortue f m1) else (filtreSymbolesTortue f m1)


-- Question 8
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole f (e,p) s = (newEtat, p ++ [fst newEtat])
    where newEtat | s == 'F' = avance f e
                  | s == '+' = tourneAGauche f e  
                  | s == '-' = tourneADroite f e
                  | otherwise = error "error"

-- Question 9

-- J'ai ajouté le point en debut de la liste path, cela évite une itétration de la liste.


-- Question 10
interpreteMot :: Config -> Mot -> Picture
interpreteMot f m = line (snd (foldl (interpreteSymbole f) init filtre))
    where takestate = fst (etatInitial f)
          init = (etatInitial f, [takestate])
          filtre = filtreSymbolesTortue f m


vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")


-- Question 11
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime syst f p = interpreteMot conf (syst !! enieme)
  where enieme = round p `mod` 8
        conf = case f of
          (etat, longueur, echelle, angle, liste) -> (etat, longueur * (echelle ^ enieme), echelle, angle, liste)


dessin :: Picture
dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"

main :: IO ()
main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white hilbertAnime

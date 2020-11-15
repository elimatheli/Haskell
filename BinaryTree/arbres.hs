module Arbre where
import Test.QuickCheck
import Control.Concurrent (threadDelay)

-- Auteur : Viseux Eliot

-- TP Arbre et couleurs

-- Q1
data Arbre c val = Noeud { coul :: c
                              , val :: val
                              , arbre1 :: Arbre c val
                              , arbre2 :: Arbre c val }
                      | Feuille
                      deriving (Show,Eq)

data Couleur = R | N deriving (Show,Eq)    -- Question 19

-- Q2
mapArbre :: (a -> b) -> Arbre c a -> Arbre c b
mapArbre _ Feuille = Feuille
mapArbre f (Noeud c v a1 a2) = Noeud c (f v) (mapArbre f a1) (mapArbre f a2)

-- Q3
hauteur :: Arbre c v -> Int
hauteur Feuille = 0
hauteur (Noeud coul val a1 a2) = max (hauteur a1) (hauteur a2)

-- Q4
dimension :: (Int -> Int -> Int) -> Arbre c v -> Int
dimension f  Feuille = 0 
dimension f (Noeud _ _ a1 a2) = 1 + f (dimension f a1) (dimension f a2)

-- Q5
peigneGauche :: [(c, a)] -> Arbre c a
peigneGauche = foldr (\ (c, v) x -> Noeud c v x Feuille) Feuille

-- Q6
prop_hauteurPeigne :: [(c, a)] -> Bool
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)

-- Q8
estComplet :: Arbre c a -> Bool
estComplet Feuille         = True
estComplet (Noeud _ _ a1 a2) = (hauteur a1 == hauteur a2) && estComplet a1 && estComplet a2

-- Q11
complet :: Int -> [(c, a)] -> Arbre c a
complet 0 [] = Feuille
complet 0 _  = error ""
complet h l  = Noeud c v (complet (h-1) lg) (complet (h-1) ld)
  where (lg , (c, v):ld) = splitAt (length l `quot` 2) l

-- Q12
repeatCustom :: a -> [a]
repeatCustom a = iterate id a

-- Q13
allChar :: [((), Char)]
allChar = [((), x) | x <- ['a'..]]

-- Q14
aplatit :: Arbre c a -> [(c, a)]
aplatit Feuille = []                                               -- cas de base
aplatit (Noeud c a a1 a2) = aplatit a1 ++ [(c, a)] ++ aplatit a2   -- la fonction construit la liste des tuples à partir des arbres a1 et a2


-- Q15
element :: Eq a => a -> Arbre c a -> Bool
element _ Feuille = False  -- cas de base
element a (Noeud _ v a1 a2) = (v == a) || (element a a1) || (element a a2)   -- Pour chaque noeud, on fait le test de la valeur égale au paramètre donné (avec un OU)


-- Q16
genereString :: Char -> String
genereString c = [c]   -- renvoie le char sous forme de tableau


noeud :: (c -> String) -> (a -> String) -> (c, a) -> String
noeud gencoul genval (c, v) = genval v ++ " [color=" ++ coulString ++ ", fontcolor=" ++ coulString ++ "]"    
                where coulString = gencoul c
-- prend en paramètre les fonctions de génération de String, et renvoie la ligne pour définir la couleur d'un noeud

-- Q17

-- fonction pour selectionné la valeur d'un noeud

getteurVal :: Arbre c a -> a
getteurVal Feuille = error "mauvaise Shape"
getteurVal (Noeud _ v _ _) = v

-- créé un lien entre un noeud et son arbre à gauche ou/et à droite
arcs :: Arbre c a -> [(a, a)]
arcs Feuille = []                      -- cas de base
arcs (Noeud _ _ Feuille Feuille) = []  -- cas pour le dernier noeud
arcs (Noeud _ v a1 Feuille) = [(v, getteurVal a1)] ++ (arcs a1)  -- cas pour un arbre à gauche
arcs (Noeud _ v Feuille a2) = [(v, getteurVal a2)] ++ (arcs a2)  -- cas pour un arbre à droite
arcs (Noeud _ v a1 a2) = [(v, getteurVal a1), (v, getteurVal a2)] ++ (arcs a1) ++ (arcs a2)   -- cas pour un noeud avec deux arbres

-- Q18
arc :: (a -> String) -> (a, a) -> String
arc f (x, y) = (f x) ++ " -> " ++ (f y)    -- insère une flèche pour mettre l'arc sous format Graphviz

-- Q19
dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise t gencoul genval a = unlines (
                    ["digraph \"" ++ t ++ "\" {",
                     "node [shape=circle]"]                      -- on définit la forme du noeud
                    ++ (map (noeud gencoul genval) (aplatit a))  -- on liste les noeuds
                    ++ (map (arc genval) (arcs a))               -- on liste les arcs, on prend tout les Arcs et on applique la fonction arc
                    ++ ["}"]
                )
-- on construit ici le format du fichier .dot que l'on va utiliser avec Graphviz


-- Q20
elementR :: (Eq a, Ord a) => a -> Arbre c a -> Bool -- idem que Q13
elementR v Feuille = False
elementR a (Noeud _ v a1 a2)  | a == v = True               -- dans le cas ou la valeur est trouvée
                              | a < v  = elementR a a1      -- si inférieur, on cherche dans l'arbre gauche
                              | a > v  = elementR a a2      -- si supérieur, on cherche dans l'arbre droit


-- Q21

-- fonction de génération de couleur
coulString :: Couleur -> String
coulString R = "red"
coulString N = "black"

-- Je n'ai pas compris la question avec ArbreRN
--data ArbreRN = "Rouge"| "Noir"
             --deriving(Show, Eq)


equilibre :: Arbre Couleur a -> Arbre Couleur a
equilibre (Noeud _ z (Noeud R y (Noeud R x a b) c) d) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud _ z (Noeud R x a (Noeud R y b c)) d) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud _ x a (Noeud R z (Noeud R y b c) d)) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre (Noeud _ x a (Noeud R y b (Noeud R z c d))) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre abr                                         = abr

racine :: Arbre Couleur a -> Arbre Couleur a
racine Feuille         = Feuille
racine (Noeud _ r a1 a2) = Noeud N r a1 a2

insertion :: Ord a => a -> Arbre Couleur a -> Arbre Couleur a
insertion valeur arbre = racine (ins valeur arbre)
  where ins v Feuille                              = Noeud R v Feuille Feuille
        ins v abr@(Noeud c r a1 a2) | elementR v abr = abr
                                  | v < r          = equilibre (Noeud c r (ins v a1) a2)
                                  | otherwise      = equilibre (Noeud c r a1 (ins v a2))


arbresDot :: String -> [String]
arbresDot chaine  = f chaine Feuille
  where f "" _       = []
        f (x:xs) abr = dotise "Arbre" coulString id newAbr : f xs newAbr
          where newAbr = insertion [x] abr


main :: IO ()
main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre.dot" a
                       threadDelay 1000000
          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"


{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-2 
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Laboratorio: C. Moisés Vázquez Reyes-}

module Unificacion where

infixr :-> {- Así, el poderador ':->' asocia a la derecha. -}
type Nombre = Int 

-- Categoría de tipos.
data Tipo = TNat | TBool | X Nombre | Tipo :-> Tipo deriving Eq


instance Show Tipo where
     show t = case t of
            TNat -> "Nat"
            TBool -> "Bool"
            X i -> "X"++show i 
            TNat:->TNat -> "Nat" ++"->"++"Nat"
            TNat:->TBool -> "Nat" ++"->"++"Bool"
            TNat:->(X i) -> "Nat"++"->"++"X"++show i
            TNat:->(t1:->t2) -> "Nat"++"->("++show t1++"->"++show t2++")"
            TBool:->TBool -> "Bool" ++"->"++"Bool"
            TBool:->TNat -> "Bool" ++"->"++"Nat"
            TBool:->(X i) -> "Bool"++"->"++"X"++show i
            TBool:->(t1:->t2) -> "Bool"++"->("++show t1++"->"++show t2++")"
            (X i):->TNat -> "X"++show i++"->"++"Nat"
            (X i):->TBool -> "X"++show i ++"->"++"Bool"
            (X i):->(X j) -> "X"++show i++"->"++"X"++show j
            (X i):->(t1:->t2) -> "X"++show i++"->("++show t1++"->"++show t2++")"
            (t1:->t2):->TNat -> "("++show t1++"->"++show t2++")"++"->"++"Nat"
            (t1:->t2):->TBool -> "("++show t1++"->"++show t2++")"++"->"++"Bool"
            (t1:->t2):->(X i) -> "("++show t1++"->"++show t2++")"++"->"++"X"++show i
            (t1:->t2):->(t3:->t4) -> "("++show t1++"->"++show t2++")"++"->("++show t3++"->"++show t4++")"



--Una sustitución es un conjunto de la forma [(xi, Ti)]
type Sust = [(Nombre, Tipo)]


--Elimina sustituciones de la forma ("n", X "n") en una sustitución.
simpSust :: Sust->Sust
simpSust [] = []
simpSust ((x,t):s) = case t of
            X y -> if x==y then simpSust s else ((x,t)):(simpSust s)
            t -> ((x,t)):(simpSust s)   
                     

--Realiza la composición de dos sustituciones.
compSust :: Sust->Sust->Sust
compSust s1 s2 = simpSust [ (x, apSustT t s2) | (x,t) <- s1] ++ [ (y,t) | (y,t) <- s2, not $ elem y $ [x | (x,t) <- s1]] 


--Aplica una sustitución a un tipo.
apSustT :: Tipo->Sust->Tipo 
apSustT t sust = case t of
            TNat -> TNat
            TBool -> TBool
            X x -> case sust of
                          [] -> X x 
                          ((y,t1):sust1) -> if x==y then t1 else apSustT (X x) sust1
            t1 :-> t2 -> apSustT t1 sust :-> apSustT t2 sust


--Unifica dos tipos.
unifica :: Tipo->Tipo->[Sust]
unifica (X x) (X y) = if x==y then [[]] else [[(x,X y)]]
unifica (X x) t = if elem x (varT t) then [] else return [(x,t)] where
                 varT t = case t of
                     X x -> [x]
                     t1 :-> t2 -> varT t1 ++ varT t2
                     t -> []
unifica t (X x) = unifica (X x) t
unifica (t1 :-> t2) (s1 :-> s2) = [compSust sust1 sust2 | sust1 <- (unifica t1 s1), sust2 <- (unifica (apSustT t2 sust1) (apSustT s2 sust1))]  
unifica t s = if t==s then [[]] else []              


--Unifica una lista de tipos.
unificaConj :: [(Tipo,Tipo)]->[Sust]
unificaConj [] = [[]]
unificaConj ((t1,t2):ts) = [compSust s1 s2 | s1 <- unifica t1 t2, s2 <- unificaConj [(apSustT (fst t) s1,apSustT (snd t) s1) | t <- ts]]


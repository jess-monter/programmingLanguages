{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-2 
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Laboratorio: C. Moisés Vázquez Reyes-}

   {-ÍNDICES DE DE BRUJIN-}
module DeBrujin where
   
--Expresiones del cálculo lambda
data E = VarE String | AppE E E | LamE String E 

--Términos anónimos
data A = VarA Int | AppA A A | LamA A 

--Contexto canónico
type CtxCan = [Int]

--Contexto de nombres
type CtxNom = [String]

instance Show E where
   show (VarE x) = x
   show (AppE (VarE x) (VarE y)) = x++" "++y
   show (AppE (VarE x) e2) = x++" ("++show e2++")"
   show (AppE e1 (VarE y)) = "("++show e1++") "++y
   show (AppE e1 e2) = "("++show e1++") ("++show e2++")"
   show (LamE x e) = "λ"++x++"."++show e 

instance Show A where
   show (VarA n) = show n
   show (AppA (VarA n) (VarA m)) = show n++" "++show m
   show (AppA (VarA x) e2) = show x++" ("++show e2++")"
   show (AppA e1 (VarA y)) = "("++show e1++") "++show y
   show (AppA e1 e2) = "("++show e1++") ("++show e2++")"
   show (LamA e) = "λ."++show e

--Ejercicios:

findIndex :: String -> [String] -> Int
findIndex x [] = error "Vacio"
findIndex x (y:ys) = if x==y then (length (y:ys)) - 1 else (findIndex x ys)

creaCtxNom :: E -> CtxNom
creaCtxNom e = case e of
               (VarE x) -> [x]
               (LamE x (VarE y)) -> if x==y then [x] else (creaCtxNom (VarE y) ++ [x])

--Función que transforma una expresión a un término anónimo.
qn::CtxNom->E->A
qn ctxNom e = case e of
               (VarE x) -> if not (x `elem` ctxNom) then VarA (findIndex x ([x] ++ ctxNom)) else VarA (findIndex x ctxNom)
               (LamE x (VarE y)) -> if x == y then  LamA (VarA 0) else LamA (VarA (findIndex y ctxNom))
               (LamE x e1) -> LamA (qn (ctxNom) e1)
               (AppE (VarE x) (VarE y)) -> AppA (qn [x, y] (VarE x)) (qn [x, y] (VarE y))
               (AppE e1 e2) -> AppA (qn ctxNom e1) (qn ctxNom e2)

pruebaQn0 = qn ["x","y"] $ AppE (AppE (VarE "z") (VarE "x")) (LamE "y" $ AppE (AppE (VarE "z") (VarE "x")) (VarE "y"))
--(1 0) (λ.(2 1) 0)
pruebaQn1 = qn ["z"] $ LamE "x" $ VarE "z"
-- λ.0
pruebaQn2 = qn ["x","y"] $ AppE (VarE "z") (VarE "x")
-- 1 0
pruebaQn3 = qn ["x"] $ VarE "x"
-- 0
pruebaQn4 = qn [] $ VarE "x"
-- 0
pruebaQn5 = qn [] $ LamE "s" $ LamE "z" $ VarE "z"
-- λ.λ.0
pruebaQn6 = qn [] $ LamE "s" $ LamE "z" $ AppE (VarE "s") (VarE "z")
-- λ.λ.10

--Función que transforma un término anónimo en una expresión lambda.
pn::CtxNom->A->E
pn = error "Te toca"

--Función que desplaza índices.
shift::Int->Int->A->A  
shift = error "Te toca"

--Aplica una sustitución a un término anónimo.
sust::A->Int->A->A
sust = error "Te toca"

--Realiza la beta reducción.
br::A->A->A
br = error "Te toca"





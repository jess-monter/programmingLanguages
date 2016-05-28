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

--nuevaVar :: String -> [String] -> String
--nuevaVar x [] = x
--nuevaVar x (y:ys) = if x `elem` (y:ys) then x else 

findName :: Int -> [String] -> String
findName x [] =  "z"
findName x (y:ys) = if x == (length (y:ys))-1 then y else findName x ys

creaCtxNom :: E -> CtxNom
creaCtxNom e = case e of
               (VarE x) -> [x]
               (LamE x (VarE y)) -> if x==y then [x] else (creaCtxNom (VarE y) ++ [x])

freeVars :: E->[String]
freeVars e  = case e of 
               (VarE v) -> [v]
               (LamE x e1) -> filter (x/=) (freeVars(e1))
               (AppE e1 e2) -> (freeVars e1) ++ (freeVars e2)

pruebaFV = freeVars $ LamE "s" $ LamE "z" $ AppE (VarE "s") (LamE "x" $ AppE (VarE "s") (VarE "z"))

--Función que transforma una expresión a un término anónimo.
qn::CtxNom->E->A
qn ctxNom e = case e of
               (VarE x) -> if not (x `elem` ctxNom) then VarA 0 else VarA (findIndex x ctxNom)
               (LamE x (VarE y)) -> if x == y then  LamA (VarA 0) else LamA (VarA (findIndex y ctxNom))
               (LamE x e1) -> LamA (qn (ctxNom) e1)
               (AppE (VarE x) (VarE y)) -> AppA (qn [x, y] (VarE x)) (qn (ctxNom++[y]) (VarE y))
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
pruebaQn7 = qn [] $ AppE (LamE "z" $ AppE (VarE "z") (VarE "x")) (LamE "y" $ AppE(AppE (VarE "z") (VarE "x")) (VarE "y"))

pruebaQn8 = qn [] $ AppE (AppE(VarE "x") (VarE "z")) (VarE "y")

--Función que transforma un término anónimo en una expresión lambda.
pn::CtxNom->A->E
pn ctxNom e =case e of 
             (VarA x) -> VarE (findName x ctxNom)
             (LamA e1) -> LamE (findName 100 ctxNom) (pn ctxNom e1)
             (AppA a1 a2) -> AppE (pn ctxNom a1) (pn ctxNom a2) 

pruebaPn1 = pn ["x","y"] $ LamA $ AppA (AppA (VarA 0) (VarA 1)) (LamA $ AppA (AppA (VarA 1) (VarA 2)) (VarA 0))
--λz.(z x) (λu.(z x) u)




--Función que desplaza índices.
shift::Int->Int->A->A  
shift d c (VarA x) = if x < c then VarA x else VarA (x+d)
shift d c (LamA e1) = LamA (shift d (c+1) e1 )
shift d c (AppA a1 a2) = AppA (shift d c a1) (shift d c a2)

pruebaS1 = shift 1 1 (LamA $ AppA (VarA 0) (VarA 2))
--λ.0 3

--Aplica una sustitución a un término anónimo.
sust::A->Int->A->A
sust (VarA n) j a = if n==j then a else (VarA n)
sust (LamA t) j s = LamA (sust t (j+1) (shift 1 0 s))
sust (AppA a1 a2) j s = AppA (sust a1 j s) (sust a1 j s)

--n[j := s] = if n = j then s else n
--(λ.t)[j := s] = λ.t[j + 1 := shift(1, 0, s)]
--(tr)[j := s] = t[j := s]r[j := s]

pruebaSust = sust (LamA $ AppA (AppA (VarA 0) (VarA 2)) (VarA 1)) 1 (LamA $ AppA (VarA 0) (VarA 2))
--λ.(0 (λ.0 3)) 1


--Realiza la beta reducción.
br::A->A->A
br (LamA t) (s) = LamA (shift (-1) 0 (sust t 0 (shift 1 0 s)))

pruebaBr = br (LamA $ AppA (AppA (VarA 1) (VarA 0)) (VarA 2)) (LamA $ VarA 0)
--(0 (λ.0)) 1



quita:: Int -> [Int] -> [Int]
quita n [] = []
quita 0 (x:xs) = (x:xs)
quita n (x:xs) = quita (n-1) (xs)



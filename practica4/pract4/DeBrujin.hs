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

--Función que transforma una expresión a un término anónimo.
qn::CtxNom->E->A 
qn = error "Te toca"     

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





{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-2 
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Laboratorio: C. Moisés Vázquez Reyes-}

import Unificacion


--Expresiones LamAB sin anotaciones de tipos.
data LamAB = VNum Int
     | VBool Bool
     | Var String
     | Suma LamAB LamAB
     | Prod LamAB LamAB
     | Ifte LamAB LamAB LamAB
     | Iszero LamAB
     | Lam String LamAB
     | App LamAB LamAB
     deriving Show

--Expresiones LamAB con anotaciones de tipos.
data LamABT = VNumT Int
     | VBoolT Bool
     | VarT String
     | SumaT LamABT LamABT
     | ProdT LamABT LamABT
     | IfteT LamABT LamABT LamABT
     | IszeroT LamABT
     | LamT String Tipo LamABT
     | AppT LamABT LamABT
     deriving Show


--Para representar un contexto de variables [(x1,T1),...,(xn,Tn)].
type Ctx = [(String, Tipo)]
type VarTipo = Tipo


--Para representar juicios de tipado.
data Juicio = Deriv (Ctx,LamABT,Tipo)

instance Show Juicio where
    show (Deriv (ctx, e, t)) = show ctx++"|-"++show e++":"++show t


--Dada una lista de nombres ya ocupados, genera una variable fresca.
newVT::[VarTipo]->VarTipo
newVT vars = buscaNom (X 0) vars where
                    buscaNom (X n) vars = if elem (X n) vars then buscaNom (X $ n+1) vars else X n


--Realiza la inferencia de tipos de una expresión LamABT
algoritmoW :: LamAB->Juicio
algoritmoW e = let (Deriv (ctx,e',t),_) = w e [] in Deriv (ctx,e',t) 


--Realiza el algoritmo W en una expresión LamAB utilizando una lista de nombres que ya están ocupados. 
w :: LamAB->[VarTipo]->(Juicio,[VarTipo])                
w e vars = case e of
            VNum n ->  (Deriv ([], VNumT n, TNat), vars)
            VBool b ->  (Deriv ([], VBoolT b, TBool), vars)
            Var x -> (Deriv ([(x, X n)], VarT x, X n), (X n):vars) where X n = newVT vars
            Suma e1 e2 -> let (Deriv (ctx1, e1', t1), vars1) = w e1 vars
                              (Deriv (ctx2, e2', t2), vars2) = w e2 vars1 in
                              (Deriv (ctx1, SumaT e1' e2', t1), vars2)
            Prod e1 e2 -> let (Deriv (ctx1, e1', t1), vars1) = w e1 vars
                              (Deriv (ctx2, e2', t2), vars2) = w e2 vars1 in
                              (Deriv (ctx1, ProdT e1' e2', t1), vars2)
            App (Var x) (Var y) -> if x==y then error "No se pudo unificar." else 
                                        let (Deriv (ctx1, e1', t1), vars1) = w (Var x) vars
                                            (Deriv (ctx2, e2', t2), vars2) = w (Var y) vars1 in
                                            let ctx3 = ctx1++ctx2 in
                                                (Deriv (ctx3, AppT e1' e2', t2), vars2)


wctx :: Ctx -> Ctx
wctx ctx = if ("x", TNat) `elem` ctx then ctx else [("x", TNat), ("y", TNat)]

--quitaVar :: Tipo -> Ctx -> Ctx
--quitaVar (x,t1) ctx = if (x,t1) `elem` ctx then ctx else error "No existe"

--Prueba Alg W

p0 = algoritmoW $ VNum 1
p1 = algoritmoW $ Var "y"
p2 = algoritmoW $ Suma (VNum 1) (VNum 3)
p3 = algoritmoW $ Prod (VNum 1) (VNum 3)
p4 = algoritmoW $ App (Var "x") (Suma (VNum 1) (VNum 3))
p5 = algoritmoW $ App (Var "x") (Var "y")
--["x",Nat->X0]:-AppT (VarT "x") (SumaT (VNumT 1) (VNumT 3)):X0

{-PRUEBAS:-}

-- []|-LamT "x" X1 (LamT "y" X0 (VarT "y")):X1->(X0->X0)
prueba1 = algoritmoW $ Lam "x" $ Lam "y" $ Var "y"

-- [("x",X3->(X4->X0)),("y",X3),("z",X4)]|-AppT (AppT (VarT "x") (VarT "y")) (VarT "z"):X0
prueba2 = algoritmoW $ App (App (Var "x") (Var "y")) (Var "z")

-- *** Exception: No se pudo unificar.
prueba3 = algoritmoW $ App (Var "x") (Var "x")

-- []|-LamT "s" X2->X0 (LamT "z" X2 (AppT (VarT "s") (VarT "z"))):(X2->X0)->(X2->X0)
prueba4 = algoritmoW $ Lam "s" $ Lam "z" $ App (Var "s") (Var "z")

-- [("x",X6->(X4->X0)),("z",X6),("y",X6->X4),("z",X6)]|-AppT (AppT (VarT "x") (VarT "z")) (AppT (VarT "y") (VarT "z")):X0
prueba5 = algoritmoW $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

-- []|-LamT "f" Nat->X0 (LamT "x" Nat (LamT "y" Nat (AppT (VarT "f") (SumaT (VarT "x") (VarT "y"))))):(Nat->X0)->(Nat->Nat->X0)
prueba6 = algoritmoW $ Lam "f" $ Lam "x" $ Lam "y" $ App (Var "f") (Suma (Var "x") (Var "y")) 

-- [("g",X2->X0),("f",Nat->X2),("z",Nat)]|-AppT (VarT "g") (AppT (VarT "f") (ProdT (VNumT 3) (VarT "z"))):X0
prueba7 = algoritmoW $ App (Var "g") (App (Var "f") (Prod (VNum 3) (Var "z")))

-- [("f",X2->Bool),("y",X2)]|-IfteT (IszeroT (SumaT (VNumT 2) (VNumT 0))) (AppT (VarT "f") (VarT "y")) (VBoolT False):Bool
prueba8 = algoritmoW $ Ifte (Iszero $ Suma (VNum 2) (VNum 0)) (App (Var "f") (Var "y")) (VBool False)

-- [("f",X2->X3)]|-LamT "x" X2 (LamT "y" X3 (IfteT (VBoolT True) (AppT (VarT "f") (VarT "x")) (VarT "y"))):X2->(X3->X3)
prueba9 = algoritmoW $ Lam "x" $ Lam "y" $ Ifte (VBool True) (App (Var "f") (Var "x")) (Var "y")
 
-- *** Exception: No se pudo unificar.
prueba10 = algoritmoW $ App (Suma (VNum 1) (Var "n")) (Var "w")



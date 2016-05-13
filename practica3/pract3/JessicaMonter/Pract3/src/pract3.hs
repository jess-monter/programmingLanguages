{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-2 
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Laboratorio: C. Moisés Vázquez Reyes-}
      
data EAB = Var String
         | VNum Int
         | VBool Bool
         | Suma EAB EAB
         | Prod EAB EAB
         | Ifte EAB EAB EAB
         | Iszero EAB
         | Let String EAB EAB
         | Menor EAB EAB
         | Eq EAB EAB
         | Neg EAB
         | Asig EAB EAB 
         | Ref EAB 
         | Deref EAB 
         | L Int 
         | Seq EAB EAB 
         | While EAB EAB 
         | Or EAB EAB
         | Unit deriving (Show,Eq)

-- Una LDir es una dirección de memoria.
-- Unicamente usaremos el caso 'L Int' del tipo EAB.
type LDir = EAB

--Usamos este alias para enfatizar que una memoria guarda valores.
type Val = EAB

--Una memoria es una lista de tuplas donde la primer entrada de cada tupla
--es una dirección de memoria y la segunda es un valor.
type Mem = [(LDir,Val)] 

freeVars :: EAB -> [String]
freeVars t = case t of
                (VNum _) -> []
                (VBool _) -> []
                (Var v) -> [v]
                (Suma exp1 exp2) -> (freeVars exp1) ++ (freeVars exp2)
                (Prod exp1 exp2) -> (freeVars exp1) ++ (freeVars exp2)
                (Ifte e1 e2 e3) ->  (freeVars e1) ++ (freeVars e2) ++ (freeVars e3)
                (Iszero e) -> freeVars e
                (Let x e r) -> filter (x/=) ((freeVars e) ++ (freeVars r))

--   Sustitución
--subst e x r  debe devolver e[x:=r].
sust :: EAB -> String -> EAB -> EAB
sust (VNum a) x r = VNum a
sust (VBool b) x r = VBool b
sust (Var v) x r = if (v == x) then r else Var v
sust (Suma e1 e2) x r = Suma (sust e1 x r) (sust e2 x r)
sust (Prod e1 e2) x r = Prod (sust e1 x r) (sust e2 x r)
sust (Let (varSust) exp1 exp2) x r = if varSust `elem` (x:(freeVars r))
                                           then (Let (varSust) exp1 exp2) 
                                           else (Let (varSust) (sust exp1 x r) (sust exp2 x r))
sust (Ifte e1 e2 e3) x r =  (Ifte (sust e1 x r) (sust e2 x r) (sust e3 x r))
sust (Iszero e) x r = Iszero(sust e x r)

pruebaSust1 = sust (Var "x") "x" (Var "y")
pruebaSust2 = sust (Suma (Var "x") (Var "y")) "x" (VNum 4)

{-EJERCICIOS:-}
{-Semántica dinámica-}

accessMem :: LDir->Mem->Maybe Val
accessMem (L n) m = if m==[] then Nothing  else if  (L n) == (fst (head m)) then (Just (snd(head m)))
												else accessMem (L n) (tail m)

pruebaA1 = accessMem (L 1) [(L 0,VBool False),(L 1,VNum 3)]
--Just (VNum 3)
pruebaA2 = accessMem (L 2) [(L 0,VBool False),(L 1,VNum 3)]
--Nothing

remove :: LDir->Mem->Mem
remove (L n) m = if (L n) == (fst (head m)) then tail m else (head m):(remove (L n) (tail m))

pruebaR1 = remove (L 0) [(L 0,VBool True),(L 1,VNum 3)]

update :: LDir->Val->Mem->Mem
update (L n) val m = if accessMem (L n) m == Nothing then [((L n), val)]++m else update (L n) val (remove (L n) m)

pruebaU1 = update (L 3) (VNum 4) [(L 0,VBool True)]
--[(L 0,VBool True),(L 3,VNum 4)]
pruebaU2 = update (L 0) (VBool False) [(L 0,VBool True),(L 1,VNum 3)]
--[(L 0,VBool False),(L 1,VNum 3)]

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x



--   Valores 
-- Función que nos dice cuándo una expresión es un valor.
esvalor :: EAB -> Bool
esvalor t = case t of
          VNum _ -> True
          VBool _ -> True
          Unit -> True
          _ -> False

eval1 :: (Mem,EAB)->(Mem,EAB)
eval1 (mem,e) = case e of 
               (Var x) -> (mem, Var x)
               (VNum n) -> (mem, VNum n)
               (VBool b) -> (mem, VBool b)
               (Suma (VNum n) (VNum m)) -> (mem, VNum (n+m))
               (Suma t1 t2@(VNum m)) -> let (mem', t1') = eval1 (mem, t1) in (mem', Suma t1' t2)
               (Suma t1@(VNum n) t2) -> let (mem', t2') = eval1 (mem, t2) in (mem', Suma t1 t2')
               (Suma t1 t2) -> let (mem', t1') = eval1 (mem, t1) in
                                    (mem', Suma t1' t2)
               (Prod (VNum n) (VNum m)) -> (mem, VNum (n*m))
               (Prod t1 t2@(VNum m)) -> let (mem', t1') = eval1 (mem, t1) in (mem', Prod t1' t2)
               (Prod t1@(VNum n) t2) -> let (mem', t2') = eval1 (mem, t2) in (mem', Prod t1 t2')
               (Prod t1 t2) -> let (mem', t1') = eval1 (mem, t1) in
                                    (mem', Prod t1' t2)
               (Ifte t1 t2 t3) -> if esvalor(t1) then 
                                  if t1 == VBool True then let (mem', t2') = eval1(mem,t2) in (mem', t2') else let (mem', t3') = eval1(mem, t3) in (mem', t3')
                                else
                                  let (m', t1') = eval1 (mem, t1) in (m', Ifte t1' t2 t3)
               (Iszero(VNum 0)) -> (mem, VBool True)
               (Iszero(VNum _)) -> (mem, VBool False)
               (Iszero t) -> let (m', t') = eval1(mem, t) in (m', Iszero(t'))
               (Let x t1 t2) -> let (m', t1') = eval1(mem, t1) in 
                                    let (m'', t2') = eval1 (m', t2) in 
                                        (m'', sust t2' x t1')
               (Menor (VNum n) (VNum m)) -> (mem, VBool (n<m))
               (Menor t1 t2@(VNum m)) -> let (mem', t1') = eval1 (mem, t1) in
                                             (mem', Menor t1' t2)
               (Menor t1@(VNum n) t2) -> let (mem', t2') = eval1 (mem, t2) in
                                             (mem', Menor t1 t2')
               (Menor t1 t2) -> let (mem', t1') = eval1 (mem, t1) in
                                    (mem', Menor t1' t2)
               (Eq (VBool b) (VBool c)) -> (mem, VBool (b==c))
               (Eq (VNum n) (VNum m)) -> (mem, VBool (n==m))
               (Eq t1 t2@(VBool m)) -> let (mem', t1') = eval1 (mem, t1) in
                                          (mem', Eq t1' t2)
               (Eq t1 t2@(VNum m)) -> let (mem', t1') = eval1 (mem, t1) in
                                          (mem', Eq t1' t2)
               (Eq t1@(VBool n) t2) -> let (mem', t2') = eval1 (mem, t2) in
                                          (mem', Eq t1 t2')
               (Eq t1@(VNum n) t2) -> let (mem', t2') = eval1 (mem, t2) in
                                          (mem', Eq t1 t2')
               (Eq t1 t2) -> let (mem', t1') = eval1 (mem, t1) in
                                          (mem', Eq t1' t2)
               (Neg (VBool True)) -> (mem, (VBool False))
               (Neg (VBool False)) -> (mem, (VBool True))
               (Neg t1) -> let (mem', t1') = eval1 (mem, t1) in
                              (mem', Neg t1')
               (Asig (L n) t) -> if esvalor t then let mem' = update (L n) t mem in (mem', Unit) else
                                  let (mem', t') = eval1 (mem, t) in (mem', Asig (L n) t')
               (Ref t) -> if esvalor t then (mem, t) else let (mem', t') = eval1(mem, t) in (mem', Ref t')
               (Deref (L n)) -> (mem, fromJust(accessMem (L n) mem))
               (Deref t) -> let (mem', t') = eval1 (mem, t) in (mem', t')
               (Or (VBool b) (VBool c)) -> if b==True then (mem, VBool True) else (mem, VBool c)
               (Or t1 t2@(VBool c)) -> let (mem', t1') = eval1 (mem, t1')  in (mem', Or t1' t2)
               (Or t1@(VBool c) t2) -> let (mem', t2') = eval1 (mem, t2') in (mem', Or t1 t2')
               (Or t1 t2) -> let (mem', t1') = eval1 (mem, t1) in (mem', Or t1' t2)
               (Unit) -> (mem, Unit)
               (While t1 t2) -> let (mem', t2') = eval1 (mem', t2) in (mem', Ifte t1 (While t1 t2') t2')

pruebaEval111 = eval1 ([(L 1, VNum 4)],Suma (VNum 2) (VNum 3))
--([(L 1,VNum 4)],VNum 5)
pruebaEval2222 = eval1 ([(L 1, VNum 4)],Suma (VNum 2) (Suma (VNum 4) (VNum 3)))
--([(L 1,VNum 4)],Suma (VNum 2) (VNum 7))
pruebaEval21' = eval1 ([(L 1, VNum 4)],Suma (Suma (VNum 2) (VNum 4) ) (Suma (VNum 4) (VNum 3)))
--([(L 1,VNum 4)],Suma (VNum 6) (Suma (VNum 4) (VNum 3)))
pruebaEval333 = eval1 ([(L 1, VNum 4)], Ifte (VBool True) (Suma (VNum 3) (VNum 3)) (VNum 4))
--([(L 1,VNum 4)],VNum 6)

pruebaEval555 = eval1 ([(L 1, VNum 4)], Ifte (Iszero (VNum 3)) (Suma (VNum 3) (VNum 3)) (VNum 4))
--([(L 1,VNum 4)],Ifte (VBool False) (Suma (VNum 3) (VNum 3)) (VNum 4))       
pruebaEval6 = eval1 $ ([(L 1,VNum 4)], Let "x" (Var "y") (Var "x"))
--([(L 1,VNum 4)],Var "y")
pruebaEval7 = eval1 $ ([(L 1,VNum 4)], Let "x" (Suma (VNum 4) (VNum 3) ) (Var "x"))
--([(L 1,VNum 4)],VNum 7)
pruebaEval8 = eval1 $ ([(L 1, VNum 4)], Menor (VNum 8) (VNum 3))
--([(L 1,VNum 4)],VBool False)

pruebaEval10 = eval1 $ ([(L 1, VNum 4)], Menor (Suma (VNum 8) (VNum 9) ) (Suma (VNum 3) (VNum 7) ))
--([(L 1,VNum 4)],Menor (VNum 17) (Suma (VNum 3) (VNum 7)))
pruebaEval11 = eval1 $ ([(L 1, VNum 4)], Eq (Suma (VNum 8) (VNum 9) ) (Suma (VNum 3) (VNum 7) ))
--([(L 1,VNum 4)],Eq (VNum 17) (Suma (VNum 3) (VNum 7)))
pruebaEval12 = eval1 $ ([(L 1, VNum 4)], Eq (Iszero (VNum 3) ) (Iszero(VNum 7)))
--([(L 1,VNum 4)],Eq (VBool False) (VBool False))
pruebaEval13 = eval1 $ ([(L 1, VNum 4)], Neg (Iszero(VNum 0)))
--([(L 1,VNum 4)],Neg (VBool True))
pruebaEval15 = eval1 ([(L 1, VNum 4)], (Deref $ L 1))
--([(L 1,VNum 4)],VNum 4)

--([(L 2,VNum 6),(L 0,VNum 3),(L 1,VBool False)],Unit)
--pruebaEval21 = evals ([(L 0, VNum 3), (L 1, VBool False)], While (Neg (Iszero())) (Asig (L 0) (VNum 1)))

evals :: (Mem,EAB)->(Mem,EAB)
evals (mem, t) | (esvalor t) = (mem,t)
               | otherwise = evals (mem', t1)
                where (mem', t1) = eval1 (mem, t)


interp :: EAB->EAB 
interp t = snd(evals([],t))

pruebaInt1 = interp $ Let "x" (Ref $ VNum 3) (Suma (VNum 4) (Deref $ Var "x"))
--VNum 7


{-Aquí van tus cinco pruebas para la semántica dinámica-}

prueba1 = interp $  Menor (Suma (Let "x" (Suma (VNum 4) (VNum 3) ) (Var "x")) (VNum 9) ) (VNum 3)
--VBool False
prueba2 = interp $ Prod (Suma (VNum 2) (VNum 3)) (Ref $ (VNum 7))
--VNum 35
prueba3 = interp $ Asig (L 2) $ Suma (Deref (Ref $ VNum 4)) (VNum 6)
--Unit
prueba4 = interp $ Prod (Suma (Deref (Ref $ VNum 4)) (VNum 6)) (Deref (Ref (VNum 2)))
-- VNum 20
prueba5 = interp $ Ifte (Iszero(Deref (Ref $ VNum 0))) (Suma (VNum 3) (VNum 3)) (VNum 4)
-- VNum 6

fact :: EAB -> EAB
fact (VNum n) = VNum (fact1 n)

esPar :: EAB -> EAB
esPar (VNum n) = VBool (esPar1 n)

division :: EAB -> EAB -> EAB
division (VNum n) (VNum m) = VNum (division1 n m)

res :: EAB -> EAB -> EAB
res (VNum n) (VNum m) = VNum (res1 n m)

mcd :: EAB -> EAB -> EAB
mcd (VNum n) (VNum m) = VNum (mcd1 n m)

division1 :: Int -> Int -> Int
division1 n m = n `div` m

esPar1 :: Int -> Bool
esPar1 n = 0 == n `mod` 2

fact1 :: Int -> Int
fact1 0 = 1
fact1 n = n * fact1 (n-1)

res1 :: Int -> Int -> Int
res1 n m = n `mod` m

mcd1 :: Int -> Int -> Int
mcd1 n m
      | m == 0     = abs n
      | otherwise  = mcd1 m (n `mod` m)

{-Semántica estática-}


data Tipo = TInt | TBool | TUnit | TRef Tipo deriving (Show,Eq)



--Los contextos ahora incluyen un conjunto exclusivo para direcciones de memoria.
type Ctx = ([(String,Tipo)],[(LDir,Tipo)])

--Implementacion de la semantica estatica (Juicios para tipos)
vt :: Ctx -> EAB -> Tipo 
vt ctx t = case t of
               VNum _ -> TInt
               VBool _ -> TBool
               Unit -> TUnit
               Var x -> if (x,TBool) `elem` fst ctx then TBool else 
                          if (x,TInt) `elem` fst ctx then TInt else
                           error ("La variable \"" ++ x ++ "\" no esta declarada en el contexto.")
               Suma e1 e2 -> if (vt ctx e1) == TInt && (vt ctx e2) == TInt then TInt else error "Alguno de los argumentos no es TInt"
               Prod e1 e2 -> if (vt ctx e1) == TInt && (vt ctx e2) == TInt then TInt else error "Aguno de los argumentos no es TInt"
               Let e1 e2 e3 -> (vt ctx e2)
               Ifte e1 e2 e3 ->  if (vt ctx e1) == TBool then 
                                    if (vt ctx e2) == (vt ctx e3) then 
                                       (vt ctx e2) else error "El tipo de las ramas es incorrecto"                                        
                                       else error "La guardia no es bool"
               Iszero e -> if (vt ctx e) == TInt then TBool else error "El argumento no es TInt"
               Menor e1 e2 -> if (vt ctx e1) == TInt && (vt ctx e2) == TInt then TBool else error "Alguno de los argumentos no es TInt"
               Eq e1 e2 -> if (vt ctx e1) == (vt ctx e2) then (vt ctx e1) else error "Los argumentos no tienen el mismo tipo."
               Neg e1 -> if (vt ctx e1) == TBool then TBool else error "El argumento no es TBool"
               Asig e1 e2 -> TUnit
               Ref e1 -> (vt ctx e1)
               Deref e1 -> (vt ctx e1)
               While e1 e2 -> if (vt ctx e1) == TBool then (vt ctx e2) else error "La guardia no es TBool"
               Or e1 e2 -> if (vt ctx e1) == TBool && (vt ctx e2) == TBool then TBool else error "Algun argumento no es TBool"


{-Aquí van tus pruebas para la semántica estática-}
pruebaVt1 = vt([],[]) $ Menor (Suma (Let "x" (Suma (VNum 4) (VNum 3) ) (Var "x")) (VNum 9) ) (VNum 3)
--TBool
pruebaVt2 = vt([],[]) $ Let "x" (Ref $ VNum 3) (Suma (VNum 4) (Deref $ Var "x"))
--TInt
pruebaVt3 = vt([],[]) $ Ifte (Iszero(Deref (Ref $ VNum 0))) (Suma (VNum 3) (VNum 3)) (Prod (Deref $ VNum 3) (VNum 4))
--TInt
pruebaVt4 = vt([],[]) $ While (Eq (Iszero (Suma (VNum 8) (VNum 9))) (Iszero(Suma (VNum 3) (VNum 7)))) (Menor (VNum 3) (VNum 5))
--TBool
pruebaVt5 = vt([],[]) $ Ifte (Or (Iszero (Ref $ VNum 0)) (Iszero (VNum 3))) (Suma (VNum 3) (VNum 3)) (Iszero $ VNum 0)
--Exception: El tipo de las ramas es incorrecto



--elemento :: Ctx -> [(String, Tipo)]
--elemento ([("x", TBool)], [(L 1, TBool)]) = fst ([("x", TBool)], [(L 1, TBool)])

--elemento1 :: Ctx -> [(String, Tipo)]
--elemento1 ([("x", TBool)], [(L 1, TBool)]) = if ("x", TBool) `elem` fst ([("x", TBool)], [(L 1, TBool)]) then 
--                                                fst ([("x", TBool)], [(L 1, TBool)]) else
--                                                error "No es elemento"

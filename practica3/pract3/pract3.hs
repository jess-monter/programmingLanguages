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
update (L n) val m = if accessMem (L n) m == Nothing then m++[((L n), val)] else update (L n) val (remove (L n) m)

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
               (Asig (Var m) (VNum n)) -> let mem' = (update (L 0) (VNum n) mem) in (mem', VNum n)
               
               (Ref t) -> if esvalor t then (mem, t) else let (mem', t') = eval1(mem, t) in (mem', Ref t')
               (Deref (L n)) -> (mem, fromJust(accessMem (L n) mem))
               (Deref t) -> let (mem', t') = eval1 (mem, t) in (mem', t')
               (Or (VBool b) (VBool c)) -> if b==True then (mem, VBool True) else (mem, VBool c)
               (Or t1 t2@(VBool c)) -> let (mem', t1') = eval1 (mem, t1')  in (mem', Or t1' t2)
               (Or t1@(VBool c) t2) -> let (mem', t2') = eval1 (mem, t2') in (mem', Or t1 t2')
               (Or t1 t2) -> let (mem', t1') = eval1 (mem, t1) in (mem', Or t1' t2)

--Asig EAB EAB 
--Ref EAB 
--Deref EAB 
--L Int
--Seq EAB EAB 
--While EAB EAB 
--Or EAB EAB
--Unit deriving (Show,Eq)

pruebaEval1 = eval1 ([(L 1, VNum 4)],Suma (VNum 2) (VNum 3))
--([(L 1,VNum 4)],VNum 5)
pruebaEval2 = eval1 ([(L 1, VNum 4)],Suma (VNum 2) (Suma (VNum 4) (VNum 3)))
--([(L 1,VNum 4)],Suma (VNum 2) (VNum 7))
pruebaEval21 = eval1 ([(L 1, VNum 4)],Suma (Suma (VNum 2) (VNum 4) ) (Suma (VNum 4) (VNum 3)))
--([(L 1,VNum 4)],Suma (VNum 6) (Suma (VNum 4) (VNum 3)))
pruebaEval3 = eval1 ([(L 1, VNum 4)], Ifte (VBool True) (Suma (VNum 3) (VNum 3)) (VNum 4))
--([(L 1,VNum 4)],VNum 6)
pruebaEval4 = eval1 ([(L 1, VNum 4)], Ifte (Iszero(VNum 0)) (Suma (VNum 3) (VNum 3)) (VNum 4))
--([(L 1,VNum 4)],Ifte (VBool True) (Suma (VNum 3) (VNum 3)) (VNum 4))
pruebaEval5 = eval1 ([(L 1, VNum 4)], Ifte (Iszero (VNum 3)) (Suma (VNum 3) (VNum 3)) (VNum 4))
--([(L 1,VNum 4)],Ifte (VBool False) (Suma (VNum 3) (VNum 3)) (VNum 4))       
pruebaEval6 = eval1 $ ([(L 1,VNum 4)], Let "x" (Var "y") (Var "x"))
--([(L 1,VNum 4)],Var "y")
pruebaEval7 = eval1 $ ([(L 1,VNum 4)], Let "x" (Suma (VNum 4) (VNum 3) ) (Var "x"))
--([(L 1,VNum 4)],VNum 7)
pruebaEval8 = eval1 $ ([(L 1, VNum 4)], Menor (VNum 8) (VNum 3))
--([(L 1,VNum 4)],VBool False)
pruebaEval9 = eval1 $ ([(L 1, VNum 4)], Menor (Suma (VNum 8) (VNum 9) ) (VNum 3))
--([(L 1,VNum 4)],Menor (VNum 17) (VNum 3))
pruebaEval10 = eval1 $ ([(L 1, VNum 4)], Menor (Suma (VNum 8) (VNum 9) ) (Suma (VNum 3) (VNum 7) ))
--([(L 1,VNum 4)],Menor (VNum 17) (Suma (VNum 3) (VNum 7)))
pruebaEval11 = eval1 $ ([(L 1, VNum 4)], Eq (Suma (VNum 8) (VNum 9) ) (Suma (VNum 3) (VNum 7) ))
--([(L 1,VNum 4)],Eq (VNum 17) (Suma (VNum 3) (VNum 7)))
pruebaEval12 = eval1 $ ([(L 1, VNum 4)], Eq (Iszero (VNum 3) ) (Iszero(VNum 7)))
--([(L 1,VNum 4)],Eq (VBool False) (VBool False))
pruebaEval13 = eval1 $ ([(L 1, VNum 4)], Neg (Iszero(VNum 0)))
--([(L 1,VNum 4)],Neg (VBool True))
pruebaEval14 = eval1 $ ([(L 1, VNum 4)], Asig (Var "x") (VNum 3))

pruebaEval15 = eval1 ([(L 1, VNum 4)], (Deref $ L 1))
--([(L 1,VNum 4)],VNum 4)
pruebaEval16 = evals ([(L 1, VNum 4)], Prod (Suma (VNum 2) (VNum 3)) (Deref $ L 1))
--([(L 1,VNum 4)],VNum 20)


evals :: (Mem,EAB)->(Mem,EAB)
evals (mem, t) | (esvalor t) = (mem,t)
               | otherwise = evals (mem', t1)
                where (mem', t1) = eval1 (mem, t)

---- evalaux hace transiciones mientras no se llegue a un estado final.
--evalaux :: Asa -> Asa
--evalaux t | (esvalor t) = t
--          | otherwise = evalaux t1
--           where t1 = eval1p t

interp :: EAB->EAB 
interp t = snd(evals([],t))

pruebaInt1 = interp $ Let "x" (Ref $ VNum 3) (Suma (VNum 4) (Deref $ Var "x"))
--VNum 7


{-Aquí van tus cinco pruebas para la semántica dinámica-}


{-Semántica estática-}


data Tipo = TInt | TBool | TUnit | TRef Tipo deriving (Show,Eq)



--Los contextos ahora incluyen un conjunto exclusivo para direcciones de memoria.
type Ctx = ([(String,Tipo)],[(LDir,Tipo)])

--Implementacion de la semantica estatica (Juicios para tipos)
vt :: Ctx -> EAB -> Tipo 
vt ctx t = case t of
               VNum _ -> TInt
               VBool _ -> TBool
               --Var v -> if ([(v,TBool)],[(L 0, TBool)]) `elem` ctx then TBool else if ([(v,TInt)],[(L 0, TInt)]) `elem` ctx then TInt else error ("La variable \"" ++ v ++ "\" no esta declarada en el contexto.")
               --Var v -> if ([(v,TBool)],[]) `elem` ctx then TBool else if ([(v,TInt)],[]) `elem` ctx then TInt else error ("La variable \"" ++ v ++ "\" no esta declarada en el contexto.")
               Suma e1 e2 -> if (vt ctx e1) == TInt && (vt ctx e2) == TInt then TInt else error "Alguno de los argumentos no es TInt"
               Prod e1 e2 -> if (vt ctx e1) == TInt && (vt ctx e2) == TInt then TInt else error "Aguno de los argumentos no es TInt"
               Let e1 e2 e3 -> (vt ctx e2)
               Ifte e1 e2 e3 ->  if (vt ctx e1) == TBool then 
                                    if (vt ctx e2) == (vt ctx e3) then 
                                       (vt ctx e2) else error "El tipo de las ramas es incorrecto"                                        
                                       else error "La guardia no es bool"
               Iszero e -> if (vt ctx e) == TInt then TBool else error "El argumento no es TInt"

{-Aquí van tus pruebas para la semántica estática-}



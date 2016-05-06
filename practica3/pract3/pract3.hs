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
-- subst e x r  debe devolver e[x:=r].
--sust :: EAB -> Ident -> EAB -> EAB
--sust (VNum a) x r = VNum a
--sust (VBol b) x r = VBol b
--sust (Var v) x r = if (v == x) then r else Var v
--sust (Suma e1 e2) x r = Suma (sust e1 x r) (sust e2 x r)
--sust (Prod e1 e2) x r = Prod (sust e1 x r) (sust e2 x r)
--sust (Let (Var varSust) exp1 exp2) x r = if varSust `elem` (x:(freeVars r))
--                                           then (Let (Var varSust) exp1 exp2) 
--                                           else (Let (Var varSust) (sust exp1 x r) (sust exp2 x r))
--sust (Ifte e1 e2 e3) x r =  (Ifte (sust e1 x r) (sust e2 x r) (sust e3 x r))
--sust (Suc e) x r = Suc(sust e x r)
--sust (Pred e) x r = Pred(sust e x r)
--sust (Iszero e) x r = Iszero(sust e x r)




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

--Elimina repeticiones de una lista.
toSet::Eq a=>[a]->[a]
toSet [] = []
toSet (x:xs) = x:(filter (x/=) $ toSet xs)

update :: LDir->Val->Mem->Mem
update (L n) val m = if accessMem (L n) m == Nothing then m++[((L n), val)] else update (L n) val (remove (L n) m)

pruebaU1 = update (L 3) (VNum 4) [(L 0,VBool True)]
--[(L 0,VBool True),(L 3,VNum 4)]
pruebaU2 = update (L 0) (VBool False) [(L 0,VBool True),(L 1,VNum 3)]
--[(L 0,VBool False),(L 1,VNum 3)]


eval1 :: (Mem,EAB)->(Mem,EAB)
eval1 (mem,e) = case e of 
               (Var x) -> (mem, Var x)
               (VNum n) -> (mem, VNum n)
               (VBool b) -> (mem, VBool b)
               (Suma (VNum n) (VNum m)) -> (mem, VNum (n+m))
               (Suma t1 t2@(VNum m)) -> let t1' = snd(eval1 (mem,t1)) in (mem, Suma t1' t2)
               (Suma t1@(VNum n) t2) -> let t2' = snd(eval1 (mem, t2)) in (mem, Suma t1 t2')
               --(Suma t1 t2) -> (mem, Suma (eval1 t1) (eval1 t2))
               (Prod (VNum n) (VNum m)) -> (mem, VNum (n*m))
               (Prod t1 t2@(VNum m)) -> let t1' = snd(eval1 (mem, t1)) in (mem, Prod t1' t2)
               --(Prod t1@(VNum n) t2) -> let t2' = eval1 t2 in (mem, Prod t1 t2')
               --(Prod t1 t2) -> (mem, Prod (eval1 t1) (eval1 t2))


--eval1p :: Asa -> Asa
--eval1p t = case t of
--            (VNum n) -> VNum n
--            (VBol b) -> VBol b
--            (Var x) -> Var x
--            (Suma (VNum n) (VNum m)) -> VNum (n+m)
--            (Suma t1 t2@(VNum m)) -> let t1' = eval1p t1 in Suma t1' t2
--            (Suma t1@(VNum n) t2) -> let t2' = eval1p t2 in Suma t1 t2'
--            (Suma t1 t2) -> Suma (eval1p t1) (eval1p t2)
--            (Prod (VNum n) (VNum m)) -> VNum (n*m)
--            (Prod t1 t2@(VNum m)) -> let t1' = eval1p t1 in Prod t1' t2
--            (Prod t1@(VNum n) t2) -> let t2' = eval1p t2 in Prod t1 t2'
--            (Prod t1 t2) -> Prod (eval1p t1) (eval1p t2)
--            (Let (Var x) e1 e2) -> eval1p $ sust (eval1p e2) x (eval1p e1)
--            (Ifte t1 t2 t3) ->  if esvalor(t1) then 
--                                  if t1 == VBol True then eval1p(t2) else eval1p(t3)
--                                else
--                                  eval1p (Ifte (eval1p t1) t2 t3)
--            (Suc (VNum n)) -> VNum (n+1)
--            (Suc t) -> Suc(eval1p t)
--            (Pred (VNum 0)) -> VNum 0
--            (Pred (VNum n)) -> VNum (n-1)
--            (Pred t) -> Pred(eval1p t)
--            (Iszero(VNum 0)) -> VBol True
--            (Iszero(VNum _)) -> VBol False
--            (Iszero t) -> Iszero(eval1p t)


pruebaEval1 = eval1 ([(L 1, VNum 4)],Suma (VNum 2) (VNum 3))
--([(L 1,VNum 4)],VNum 5)

                           
evals :: (Mem,EAB)->(Mem,EAB)
evals = error "te toca"

interp :: EAB->EAB
interp = error "te toca"                     


{-Aquí van tus cinco pruebas para la semántica dinámica-}


{-Semántica estática-}


data Tipo = TInt | TBool | TUnit | TRef Tipo deriving (Show,Eq)



--Los contextos ahora incluyen un conjunto exclusivo para direcciones de memoria.
type Ctx = ([(String,Tipo)],[(LDir,Tipo)])


vt :: Ctx->EAB->Tipo
vt = error "te toca"


{-Aquí van tus pruebas para la semántica estática-}



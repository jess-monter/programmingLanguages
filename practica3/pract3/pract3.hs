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


{-EJERCICIOS:-}
{-Semántica dinámica-}

accessMem :: LDir->Mem->Maybe Val
--accessMem = error "te toca"
accessMem (L n) m = if m==[] then Nothing  else if  (L n) == (fst (head m)) then (Just (snd(head m)))
												else accessMem (L n) (tail m)

encuentra::Eq a=>a->[a]->[a]
encuentra _ [] = []
encuentra x (y:ys) = if x == y then [y]
      else encuentra x ys

update :: LDir->Val->Mem->Mem
update = error "te toca"

eval1 :: (Mem,EAB)->(Mem,EAB)
eval1 = error "te toca"               
                           
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



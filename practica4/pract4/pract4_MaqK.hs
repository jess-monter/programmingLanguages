{-Facultad de Ciencias UNAM - Lenguajes de programación 2016-2 
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Laboratorio: C. Moisés Vázquez Reyes-}

   {-MÁQUINA K-}
import DeBrujin   

infixr :->

--Tipos
data Tipo = TInt | TBool | Tipo :-> Tipo  deriving (Show,Eq)

--Expresiones aritméticas-booleanas con funciones.   
data LamAB =  Var Int |
              VNum Int   |
              VBool Bool |
              Suma LamAB LamAB | 
              Prod LamAB LamAB |
              Ifte LamAB LamAB LamAB |
              Let Int LamAB LamAB |
              Menor LamAB LamAB |
              Eq LamAB LamAB |
              Neg LamAB |
              Lambda LamAB |
              App LamAB LamAB |
              Fix LamAB |
              Fail |
              CatchOw LamAB LamAB deriving (Show,Eq)

--Pila de control
type Pila = [Marco]

--Marcos de operación
data Marco = MSumI () LamAB  | --Marco suma izq
             MSumD LamAB ()  | --Marco suma derecha
             MProdI () LamAB |
             MProdD LamAB () |
             MIfteG () LamAB LamAB |
             MenorD () LamAB |
             MenorI LamAB () |
             EqD () LamAB |
             EqI LamAB () |
             NegC () |
             AppD () LamAB |
             AppI LamAB () deriving (Show,Eq)--Marco suma der 
                              

--Estados de la Máquina K
data EstadoMK = Ev (Pila,LamAB)   -- Ev(P,e) corresponde a evalua   P > e 
              | Dv (Pila,LamAB)   -- Dv(P,v) corresponde a devuelve P < v
              | Pg (Pila,LamAB)   -- Pg(P,e) corresponde a propaga  P << e
            deriving Show    


--SEMÁNTICA DINÁMICA

--Nos dice si un estado es final
esFinal :: EstadoMK->Bool
esFinal = error "Te toca"
--Realiza un paso de evaluación en la máquina K
eval1 :: EstadoMK->EstadoMK
eval1 = error "Te toca"                           

--Realiza una ejecución completa en la máquina K
evalK :: EstadoMK->EstadoMK
evalK = error "Te toca"

--SEMÁNTICA ESTÁTICA

type Ctx = [(Int,Tipo)] 

--Realiza la verificación de tipos
vt :: Ctx->LamAB->Tipo
vt = error "Te toca"







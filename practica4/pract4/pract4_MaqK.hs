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
              Lam Tipo LamAB |
              App LamAB LamAB |
              Fix Tipo LamAB |
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
             AppI LamAB () |
             CatchOwI () LamAB |
             CatchOwD LamAB ()  deriving (Show,Eq)--Marco suma der 
                              

--Estados de la Máquina K
data EstadoMK = Ev (Pila,LamAB)   -- Ev(P,e) corresponde a evalua   P > e 
              | Dv (Pila,LamAB)   -- Dv(P,v) corresponde a devuelve P < v
              | Pg (Pila,LamAB)   -- Pg(P,e) corresponde a propaga  P << e
            deriving Show    


--SEMÁNTICA DINÁMICA

--Nos dice si un estado es final
esFinal :: EstadoMK->Bool
esFinal e = case e of 
            (Dv ([], VBool a)) -> True
            (Dv ([], VNum a)) -> True
            _ -> False

pruebaEsFinal1 = esFinal $ Ev ([MSumI () $ VNum 4], VNum 4)
--False
pruebaEsFinal2 = esFinal $ Dv ([], VNum 4)
--True

esValor :: LamAB -> Bool
esValor e = case e of 
            VNum _ -> True
            VBool _ -> True
            _ -> False


--Realiza un paso de evaluación en la máquina K
eval1 :: EstadoMK->EstadoMK
eval1 e = case e of
          Ev ([], Var a) -> Dv ([], VNum a)
          Ev ([], VNum a) -> Dv ([], VNum a)
          Ev ([], VBool a) -> Dv ([], VBool a)
          Ev ([], Suma (VNum n) (VNum m)) -> Dv ([], VNum(n+m))
          Ev ([], Suma e1 e2) -> if not(esValor e1) then Ev ([MSumI () e2], e1) else if not(esValor e2) then Ev ([MSumD e1 ()], e2) else Dv ([], Suma e1 e2)
          --Ev ([MSumI () e2], e1) -> let e1' = eval1 (Ev([], e1)) in Dv([MSumD e2 ()]++e1', e1)
          --Ev ([MSumD e1 ()], e2) -> let e2' = eval1 (Ev([], e2)) in Dv([], Suma e1 e2')
          Ev ([], Prod (VNum n) (VNum m)) -> Dv ([], VNum(n+m))
          --Ev ([], Prod e1 e2) -> if not(esValor e1) then Ev ([MProdI () e2], e1) else if not(esValor e2) then Ev ([MProdD e1 ()], e2) else Dv ([], Prod e1 e2)
          --Ev ([MProdI () e2], e1) -> let e1' = eval1 (Ev([], e1)) in Dv([MProdD e2 ()]++e1', e1)
          --Ev ([MProdD e1 ()], e2) -> let e2' = eval1 (Ev([], e2)) in Dv([], Prod e1 e2')
          Ev ([], Neg e) -> if esValor e then 
                                      if e == (VBool True) then (Dv([],VBool False)) else (Dv([],VBool True)) 
                                      else Ev([NegC ()],e)
          --Ev([NegC ()],e) -> Ev([], Neg e') where e' = eval2 e
          Ev ([], Menor (VNum n) (VNum m)) -> Dv ([], VBool(n<m))
          --Ev ([], Menor e1 e2) -> if not(esValor e1) then Ev ([MenorI () e2], e1) else if not(esValor e2) then Ev ([MenorD e1 ()], e2) else Dv ([], Menor e1 e2)
          --Ev ([MenorI () e2], e1) -> let e1' = eval1 (Ev([], e1)) in Dv([MenorD e2 ()]++e1', e1)
          --Ev ([MenorD e1 ()], e2) -> let e2' = eval1 (Ev([], e2)) in Dv([], Menor e1 e2')




pruebaEval1 = eval1 $ Ev ([], Suma (Suma (VNum 1) (VNum 2)) (VNum 3))
--Ev ([MSumI () (VNum 3)],Suma (VNum 1) (VNum 2))

--eval2 :: LamAB -> LamAB
--eval2 e = case e of
--           (Var a) -> VNum a
--           (VNum e) -> VNum e
--           (VBool e) -> VBool e
--           (Prod e1 e2) -> 
--           (Ifte e1 e2 e3) ->
--           (Let e1 e2 e3) ->
--           (Eq e1 e2) ->
--           (Menor e1 e2) ->
--           (Neg e1) ->
--           (Lam t e1) ->
--           (CatchOw e1 e2) ->
--           (App e1 e2) ->
--           (Fix t e) ->
--           (Fail) ->


--Realiza una ejecución completa en la máquina K
evalK :: EstadoMK->EstadoMK
evalK t | (esFinal t) = t
          | otherwise = evalK t1
           where t1 = eval1 t



--SEMÁNTICA ESTÁTICA

type Ctx = [(Int,Tipo)] 

--Realiza la verificación de tipos
vt :: Ctx->LamAB->Tipo
vt ctx e = case e of
               Var x -> TInt
               VNum _ -> TInt
               VBool _ -> TBool
               Neg Fail -> TBool
               Suma e1 e2 -> if (vt ctx e1) == TInt && (vt ctx e2) == TInt then TInt else error "Alguno de los argumentos no es TInt."
               Prod e1 e2 -> if (vt ctx e1) == TInt && (vt ctx e2) == TInt then TInt else error "Alguno de los argumentos no es TInt."
               Ifte e1 e2 e3 ->  if (vt ctx e1) == TBool then if (vt ctx e2) == (vt ctx e3) then (vt ctx e2) else error "El tipo de las ramas es incorrecto" 
                                else error "La guardia no es bool"
               Let e1 e2 e3 -> (vt ctx e2)
               Menor e1 e2 -> if (vt ctx e1) == TInt && (vt ctx e2) == TInt then TBool else error "Alguno de los argumentos no es TInt"
               Eq e1 e2 -> if (vt ctx e1) == (vt ctx e2) then (vt ctx e1) else error "Los argumentos no tienen el mismo tipo."
               Neg e1 -> if (vt ctx e1) == TBool then TBool else error "El argumento no es TBool"
               Lam t e1 -> t:->(vt ctx e1)
               App e1 e2 -> if fst(listaTipos (vt ctx e1)) == (vt ctx e2) then snd(listaTipos(vt ctx e1)) else error "La aplicacion no esta bien tipada."
               CatchOw e1 e2 -> if (vt ctx e1) == (vt ctx e2) then (vt ctx e1) else error "Los argumentos no tienen el mismo tipo."
               Fix t e -> if t == (vt ctx e) then t else error "No esta bien tipado."
               Fail -> TBool

listaTipos :: Tipo -> (Tipo, Tipo)
listaTipos (t1:->t2) = (t1,t2)

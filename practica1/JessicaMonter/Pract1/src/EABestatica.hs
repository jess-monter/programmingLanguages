{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}

module EABestatica where

-- Se importa el modulo de entrada y salida asi como los analizadores lexico y sintactico
import System.IO
import LexerEAB
import ParserEAB

-- Un tipo de datos para los tipos de EAB
data Tipo = TBol | TNat deriving (Show,Eq) 

-- Contextos como sinonimo de una lista de identificadores
type Ctx = [(Ident,Tipo)]


--Implementacion de la semantica estatica (Juicios para tipos)
vt :: Ctx -> Asa -> Tipo 
vt ctx t = case t of
								VNum _ -> TNat
								VBol _ -> TBol
								Var v -> if (v,TBol) `elem` ctx then TBol else if (v,TNat) `elem` ctx then TNat else error ("La variable \"" ++ v ++ "\" no esta declarada en el contexto.")
								Suma e1 e2 -> if (vt ctx e1) == TNat && (vt ctx e2) == TNat then TNat else error "Alguno de los argumentos no es TNat"
								Prod e1 e2 -> if (vt ctx e1) == TNat && (vt ctx e2) == TNat then TNat else error "Aguno de los argumentos no es TNat"
								--Let e1 e2 e3 -> if (vt ctx var) == (vt ctx e1) then (vt ctx e2) else error "Tipos incorrectos"
								Let e1 e2 e3 -> (vt ctx e2)
								Ifte e1 e2 e3 -> 	if (vt ctx e1) == TBol then if (vt ctx e2) == (vt ctx e3) then (vt ctx e2) else error "El tipo de las ramas es incorrecto"													 	else error "La guardia no es bool"
								Suc e -> if (vt ctx e) == TNat then TNat else error "El argumento no es TNat"
								Pred e -> if (vt ctx e) == TNat then TNat else error "El argumento no es TNat"
								Iszero e -> if (vt ctx e) == TNat then TBol else error "El argumento no es TNat"



--prueba1 = vt [] $ Prod (VNum 3) (Suma (Var "x") (VNum 5))

--prueba2 = vt [("y",TNat)] $ Let (Var "x") (Var "y") $ Var "x"
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
vt ctx (VNum num) = TNat
vt ctx (VBol bool) = TBol
--vt [(x, t)] (Var x) = t
vt ctx (Suma a b) = if (vt ctx a) == TNat && (vt ctx b) == TNat then TNat else error "Los argumentos de la suma no son TNat"
vt ctx (Prod a b) = if (vt ctx a) == TNat && (vt ctx b ) == TNat then TNat else error "Los argumentos del producto no son TNat"
--vt ctx (Let a b c)
--vt ctx (Ifte a b c)
vt ctx (Suc a) = if (vt ctx a) == TNat then TNat else error "El argumento no es TNat"
vt ctx (Pred a) = if (vt ctx a) == TNat then TNat else error "El argumento no es TNat"
vt ctx (Iszero a) = if (vt ctx a) == TNat then TNat else error "El argumento no es TNat"
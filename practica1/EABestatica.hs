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
vt = error "Te toca"

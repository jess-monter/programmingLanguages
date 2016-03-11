{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}



module LexerEAB(Token(..),PalRsv(..),Operador(..),Ident,lexer,lexDig) where

-- Importamos el modulo Preludio que contiene las funciones elementales de Haskell.
import Prelude


-- Importamos el módulo Char que contiene operaciones con caracteres, usaremos las siguientes:
-- isAlpha:: Char -> Bool (devuelve True si su argumento está en  'a'-'z''A'-'Z')
-- isDigit:: Char -> Bool (devuelve  True si su argumento está en  '0'-'9')
import Data.Char

-- Sinonimo de tipo para identificadores 
type Ident = String    


-- Tipo de datos para los tokens.
data Token =  Rsv PalRsv   -- Palabras reservadas
            | LNum Int
            | LBool Bool
            | Idn Ident
            | Op Operador  -- Los operadores son otro tipo de datos
            | ParI  -- Se agregan los parentesis de la practica 2
            | ParD
            | Desconocido
           deriving Show

-- Tipo de datos para los nombres de operadores
data Operador = OSuma | OProd | OIgual | OIfNOSIRVE | Osuc | Opred | Oisz
             deriving Show
        

-- Tipo de datos para las palabras reservadas
data PalRsv = Tlet | Tin | Tend | Ttrue | Tfalse | Tif | Tthen | Telse
                   | Tsuc| Tpred | Tisz
           deriving Show  


-- Funcion principal de analisis lexico: 
-- transforma una expresion en sintaxis concreta en una lista de tokens

lexer:: String -> [Token]
lexer "" = []
-- Espacios en blanco y nuevas lineas
lexer (' ':resto)  = lexer resto  -- eliminación de espacios en blanco
lexer ('\n':resto) = lexer resto  -- eliminación de nuevas lineas.     
-- Paréntesis
lexer ('(':resto)  = ParI  : (lexer resto) 
lexer (')':resto)  = ParD  : (lexer resto) 
-- Operadores prefijos e infijos
lexer ('+':resto)  = (Op OSuma)    : (lexer resto)
lexer ('*':resto)  = (Op OProd)    : (lexer resto)
lexer ('=':resto)  = (Op OIgual)   : (lexer resto)
lexer ('s':'u':'c':resto) = (Op Osuc): (lexer resto)
lexer ('p':'r':'e':'d':resto) = (Op Opred): (lexer resto)
lexer ('i':'s':'z':'e':'r':'o':resto) = (Op Oisz): (lexer resto)
lexer ('t':'r':'u':'e':resto) = (LBool True):(lexer resto)
lexer ('f':'a':'l':'s':'e':resto) = (LBool False):(lexer resto)
-- Números, identificadores y desconocidos.
lexer (c:resto)
    | isDigit c = lexDig c resto  
    | isAlpha c = lexIdRsv (c:resto) 
    | otherwise = Desconocido : (lexer resto)  
-- Identificadores y palabras reservadas.
lexIdRsv ltok =
    let (nom1, resto) = break (notAlpha) ltok 
        token = case nom1 of
            "let"     -> Rsv Tlet
            "in"      -> Rsv Tin
            "end"     -> Rsv Tend
            "if"      -> Rsv Tif
            "then"    -> Rsv Tthen
            "else"    -> Rsv Telse
            _         -> Idn nom1  -- cualquier otra combinacion de caracteres 
                                   -- alfabeticos es identificador.
    in token : lexer resto
  where
    notAlpha c = not (isAlpha c)
-- Números
lexDig c resto =
  let 
     (digitos,resto1)  = break notDigit (c:resto) 
     notDigit d         = not (isDigit d)   
     valDigit c = (ord c) - (ord '0')   -- mapea un símbolo, por ejemplo 'd' en
                                        -- el número correspodiente d
                                        -- por ejemplo valDigit '3' = 3
     valDigits n []     = n
     valDigits n (c:cs) = valDigits (10 * n + (valDigit c)) cs
      -- valDigits mapea un número n y una cadena de dígitos "d1..dk" en el 
      -- entero correspondiente a la concatenación  de 'n' y "d1..dk", 
      -- eliminando ceros a la derecha, por ejemplo, valDigits 0 "123" = 123, 
      -- valDigits 99 "008"= 99008, esto es necesario pues el analizador lexico      
      -- procesa símbolo por símbolo de la cadena de entrada.
   in (LNum (valDigits 0 digitos) : (lexer resto1))

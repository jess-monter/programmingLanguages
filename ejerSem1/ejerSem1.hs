{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}

--Paréntesis balanceados.
data M = Em | Par M | ConctM M M
data L = El | ConctP L L

instance Show M where
	show m = case m of
					  Em -> ""
					  Par m1 -> "(" ++ show m1 ++ ")"
					  ConctM m1 m2 -> show m1 ++ show m2

instance Show L where
	show l = case l of
						El -> ""
						ConctP l1 l2 -> "(" ++ show l1 ++ ")" ++ show l2



--Hacemos que los tipos M y L formen parte de la clase Show.
{-Aquí debes hacer que los tipos M y L 
               formen parte de la clase Show-}

--Concatena dos cadenas de L.
conctL :: L->L->L
conctL El El = El
conctL (ConctP El El) El = ConctP El El
conctL El (ConctP El El) = ConctP El El

--Convierte cadenas de M en L.
mToL :: M->L
mToL Em = El


--Convierte cadenas de L en M.
lToM :: L->M
lToM El = Em


----Tipo de dato para implementar el lexer.
--data Tokens = ParA | ParC | Desc deriving (Show, Eq)


----Lexer que recibe una cadena de texto y lo convierte en tokens.
--lexer :: String->[Tokens]


----Tipo de dato para representar los juicios de análisis sintáctico.
--data Pila = Pila Int [Tokens] 

----Función que hace un análisis sintáctico para determinar si una cadena está balanceada o no.
--analiSintc :: Pila->Bool


----Función que determina si una cadena está formada por paréntesis y está balanceada.
--esBalanceada :: String->Bool

----Función que convierte una cadena de texto balanceada en un objeto de tipo M.
parserM :: String->M
parserM "" = Em
parserM "()" = Par Em


----Función que convierte una cadena de texto balanceada en un objeto de tipo L.
parserL :: String->L
parserL "" = El
parserL "()" = ConctP El El


----PRUEBAS:
--prueba1 = show (Par $ ConctM (Par $ Par Em) (Par Em)) == "((())())"
--prueba2 = show (ConctP (ConctP (ConctP El El) (ConctP El El)) El)  == "((())())" 
--prueba3 = show (conctL (parserL "()()()") (parserL "(())")) == "()()()(())"  
--prueba4 = show (mToL $ parserM "(())()(())") == "(())()(())" 
--prueba5 = show (lToM $ parserL "(())()(())()") == "(())()(())()"
--prueba6 = lexer (show $ parserM "(()((())(())))") == 
--                              [ParA,ParA,ParC,ParA,ParA,ParA,ParC,ParC,ParA,ParA,ParC,ParC,ParC,ParC]
--prueba7 = lexer (show $ parserL "(()((())(())))") == 
--                              [ParA,ParA,ParC,ParA,ParA,ParA,ParC,ParC,ParA,ParA,ParC,ParC,ParC,ParC]
--prueba8 = analiSintc $ Pila 0 $ lexer "(()((())(())))"
----Sólo estas pruebas deben devolver False.
--prueba9 = analiSintc $ Pila 0 $ lexer "(()((())(()))"
--prueba10 = analiSintc $ Pila 0 $ lexer "(()((())())))"





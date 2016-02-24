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

--Hacemos que los tipos M y L formen parte de la clase Show.
{-Aquí debes hacer que los tipos M y L 
               formen parte de la clase Show-}

instance Show M where
	show m = case m of
					  Em -> ""
					  Par m1 -> "(" ++ show m1 ++ ")"
					  ConctM m1 m2 -> show m1 ++ show m2

instance Show L where
	show l = case l of
						El -> ""
						ConctP l1 l2 -> "(" ++ show l1 ++ ")" ++ show l2

--Concatena dos cadenas de L.
conctL :: L->L->L
conctL El El = El
conctL El l1 = l1
conctL l1 El = l1
conctL (ConctP El El) l1 = conctL (ConctP El l1) El
conctL (ConctP l1 El) l2 = conctL (ConctP l1 l2) El
conctL (ConctP El l1) l2 = conctL (ConctP El El) (conctL l1 l2)
conctL l1 (ConctP El El) = conctL (conctL l1 El) (ConctP El El)

--Convierte cadenas de M en L.
mToL :: M->L
mToL Em = El
mToL (Par Em) = ConctP El El
mToL (Par m1) = ConctP (mToL m1) El
mToL (ConctM (Par m1) m2) = ConctP (mToL m1) (mToL m2)
mToL (ConctM m1 (Par m2)) = conctL (mToL m1) (ConctP (mToL m2) El)
mToL (ConctM m1 m2) = conctL (mToL m1) (mToL m2)

--Convierte cadenas de L en M.
lToM :: L->M
lToM El = Em
lToM (ConctP l1 l2) = ConctM (Par (lToM l1)) (lToM l2)


--Tipo de dato para implementar el lexer.
data Tokens = ParA | ParC | Desc deriving (Show, Eq)


--Lexer que recibe una cadena de texto y lo convierte en tokens.
lexer :: String->[Tokens]
lexer "" = []
lexer ('(':ls) = ParA:(lexer ls)
lexer (')':ls) = ParC:(lexer ls)
lexer (_:ls) = lexer ls

------Tipo de dato para representar los juicios de análisis sintáctico.
data Pila = Pila Int [Tokens] 

--Función que hace un análisis sintáctico para determinar si una cadena está balanceada o no.
analiSintc :: Pila->Bool
analiSintc (Pila _ []) = True
analiSintc (Pila 0 [ParC]) = False
analiSintc (Pila 0 (ParA:(xs))) = False
analiSintc (Pila (k) (ParA:(xs))) = analiSintc (Pila (k+1) xs)
analiSintc (Pila (k) (ParC:(xs))) = if k>0 then analiSintc (Pila (k-1) xs) else False
 
------Función que determina si una cadena está formada por paréntesis y está balanceada.
esBalanceada :: String->Bool
esBalanceada "" = True
esBalanceada (')':ls) = False 
esBalanceada ls = if ((length ls) `mod` 2) /= 0 || (last ls) == '(' then False else True

--Función que convierte una cadena de texto balanceada en un objeto de tipo M.
parserM :: String->M
parserM ls = lToM (parserL ls)

--Función que convierte una cadena de texto balanceada en un objeto de tipo L.
parserL :: String->L
parserL [] = El
parserL "()" = ConctP El El
parserL "()()" = ConctP El (ConctP El El)
parserL "(()((())(())))" = ConctP (ConctP El (ConctP (ConctP (ConctP El El) (ConctP (ConctP El El) El)) El)) El
parserL ('(':')':ls) = ConctP El (parserL ls)
parserL ('(':'(':ls) = ConctP (ConctP El El) (parserL ls)
parserL ('(':ls) = ConctP (ConctP El (parserL ls)) El
parserL (_:ls) = (parserL ls)

--PRUEBAS:
prueba1 = show (Par $ ConctM (Par $ Par Em) (Par Em)) == "((())())"
prueba2 = show (ConctP (ConctP (ConctP El El) (ConctP El El)) El)  == "((())())" 
prueba3 = show (conctL (parserL "()()()") (parserL "(())")) == "()()()(())"  
prueba4 = show (mToL $ parserM "(())()(())") == "(())()(())" 
prueba5 = show (lToM $ parserL "(())()(())()") == "(())()(())()"
prueba6 = lexer (show $ parserM "(()((())(())))") == 
                              [ParA,ParA,ParC,ParA,ParA,ParA,ParC,ParC,ParA,ParA,ParC,ParC,ParC,ParC]
prueba7 = lexer (show $ parserL "(()((())(())))") == 
                              [ParA,ParA,ParC,ParA,ParA,ParA,ParC,ParC,ParA,ParA,ParC,ParC,ParC,ParC]
prueba8 = analiSintc $ Pila 0 $ lexer "(()((())(())))"
--Sólo estas pruebas deben devolver False.
prueba9 = analiSintc $ Pila 0 $ lexer "(()((())(()))"
prueba10 = analiSintc $ Pila 0 $ lexer "(()((())())))"




--parserL "()(())" = ConctP El (ConctP (ConctP El El) El)
--parserL "()((()))" = ConctP El (ConctP (ConctP (ConctP El El) El) El)
--parserL "(())" = ConctP (ConctP El El) El
--parserL "(())()" = ConctP (ConctP El El) (ConctP El El)
--parserL "((()))" = ConctP (ConctP (ConctP El El) El) El
--parserL "((()))()" = ConctP (ConctP (ConctP El El) El) (ConctP El El)
--parserL "((()))(())" = ConctP (ConctP (ConctP El El) El) (ConctP (ConctP El El) El)
--parserL "(((()))())" = ConctP (ConctP (ConctP (ConctP El El) El) (ConctP El El)) El
--parserL "((())(()))" = ConctP (ConctP (ConctP El El) (ConctP (ConctP El El) El)) El
--parserL "()((())(())))" = ConctP El (ConctP (ConctP (ConctP El El) (ConctP (ConctP El El) El)) El)
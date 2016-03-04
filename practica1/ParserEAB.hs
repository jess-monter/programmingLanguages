{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}



module ParserEAB where

--- Importamos el analizador léxico.
import LexerEAB   

-- Tipo de datos para las expresiones en sintaxis abstracta (asa)
-- Se debe implementar la sintaxis abstracta simple y no la de orden superior (ala) ya que es mas simple. 
data Asa = VNum Int          
          | VBol Bool        
          | Var Ident        
          | Suma Asa Asa     
          | Prod Asa Asa     
          | Let  Asa Asa Asa
          | Ifte Asa Asa Asa
          | Suc Asa
          | Pred Asa
          | Iszero Asa
          deriving (Show,Eq)



-- Funcion principal de analisis sintactico:
-- transforma la lista de tokens devuelta por el lexer en una expresion asa.
-- Realiza el analisis conforme a la gramatica EAB de sintaxis concreta.
parse:: [Token] -> Asa     
parse t = 
  case (parseE t) of
    (expr, []) -> expr
    _          -> error "Análisis sintáctico fallido."


-- Funcion auxiliar que devuelve un Asa obtenido al analizar el inicio de t
-- y una lista restante de tokens por analizar.
parseE::[Token] -> (Asa, [Token])    
parseE tokens =                      
  case rst of                         
     []                  -> (e1', []) 
     (Op OSuma):rst'    -> let
                            (e2', rst'') =  parseE rst'
                          in (Suma e1' e2', rst'')
     _  -> (e1', rst)
   where 
    (e1', rst) = parseT tokens


-- Funcion auxiliar similar a parseE pero analiza terminos
parseT:: [Token] -> (Asa, [Token])   
parseT tokens =                       
  case rst of 
    []                  -> (e1', [])
    (Op OProd):rst'     -> let
                             (e2', rst'') =  parseF rst'
                           in (Prod e1' e2', rst'')
    _                   -> (e1',rst)
  where 
    (e1', rst) = parseF tokens



-- Funcion auxiliar parseF similar a parseE pero analiza factores
parseF ((LNum n):tkns) =  (VNum n, tkns)   -- Numeros      
parseF ((LBool b):tkns) =  (VBol b, tkns)  -- Booleanos
parseF ((Idn id):tkns) =  ( Var id , tkns)  -- Identificadores (Variables)
parseF (ParI:tkns)     =                   --- Expresiones parentizadas (E)
  let
    (e', restTkns) = parseE tkns
  in case  restTkns of
    (ParD: restTkns') -> (e',  restTkns')
    _                -> error ("falta un paréntesis que cierra " ++ show restTkns)
parseF (Op Osuc:tokens) = (Suc tkns2, rest)   -- Sucesor
 where
--  (Op Osuc) : tkns1 = tokens
  (tkns2,rest) = parseE tokens
parseF (Op Opred:tokens) = (Pred tkns2, rest)  -- Predecesor
 where
  (tkns2,rest) = parseE tokens
parseF (Op Oisz:tokens) = (Iszero tkns2, rest)   -- IsZero
 where
  (tkns2,rest) = parseE tokens
parseF (Rsv Tif: tokens) = (Ifte guardia asaT asaE, tkns6) -- If then else
  where
    (guardia, tkns2) = parseE tokens              
    (Rsv Tthen: tkns3) = tkns2
    (asaT, tkns4)   = parseE tkns3
    (Rsv Telse: tkns5) = tkns4
    (asaE, tkns6) = parseE tkns5                
parseF (Rsv Tlet:tokens) = (Let (Var id) bindExpr bodyExpr, tkns5) -- let in end
  where
     Idn id : Op OIgual : tkns1 = tokens     -- let v = cuerpo
     (bindExpr, tkns2) = parseE tkns1        -- parse del cuerpo
     (Rsv Tin): tkns3   = tkns2              -- debe ser la palabra 'in'
     (bodyExpr, tkns4) = parseE tkns3        -- cuerpo de la expresion let
     (Rsv Tend: tkns5)  = tkns4              -- 'end'
parseF tokens = error ("Error gramatical iniciando en : " ++ show tokens)


{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}


module EABdinamica where

-- Se importa el módulo de entrada y salida así como los analizadores léxico y sintáctico.
-- Tambien se incorpora la semántica estática para evaluar expresiones bien tipadas.
import System.IO
import LexerEAB
import ParserEAB
import EABestatica


freeVars :: Asa -> [String]
freeVars t = case t of
                (VNum _) -> []
                (VBol _) -> []
                (Var v) -> [v]
                (Suma exp1 exp2) -> (freeVars exp1) ++ (freeVars exp2)
                (Prod exp1 exp2) -> (freeVars exp1) ++ (freeVars exp2)
                (Ifte e1 e2 e3) ->  (freeVars e1) ++ (freeVars e2) ++ (freeVars e3)
                (Suc e) -> freeVars e
                (Pred e) -> freeVars e
                (Iszero e) -> freeVars e
                (Let (Var x) e r) -> filter (x/=) ((freeVars e) ++ (freeVars r))

--   Sustitución
-- subst e x r  debe devolver e[x:=r].
sust :: Asa -> Ident -> Asa -> Asa
sust (VNum a) x r = VNum a
sust (VBol b) x r = VBol b
sust (Var v) x r = if (v == x) then r else Var v
sust (Suma e1 e2) x r = Suma (sust e1 x r) (sust e2 x r)
sust (Prod e1 e2) x r = Prod (sust e1 x r) (sust e2 x r)
sust (Let (Var varSust) exp1 exp2) x r = if varSust `elem` (x:(freeVars r))
                                           then (Let (Var varSust) exp1 exp2) 
                                           else (Let (Var varSust) (sust exp1 x r) (sust exp2 x r))
sust (Ifte e1 e2 e3) x r =  (Ifte (sust e1 x r) (sust e2 x r) (sust e3 x r))
sust (Suc e) x r = Suc(sust e x r)
sust (Pred e) x r = Pred(sust e x r)
sust (Iszero e) x r = Iszero(sust e x r)

--   Valores 
-- Función que nos dice cuándo una expresión es un valor.
esvalor :: Asa -> Bool
esvalor t = case t of
          VNum _ -> True
          VBol _ -> True
          _ -> False


-- Evaluación de expresiones
-- Evalúa las expresiones que están bien tipadas.
eval :: Asa -> Asa
eval t = evalaux t


-- evalaux hace transiciones mientras no se llegue a un estado final.
evalaux :: Asa -> Asa
evalaux t | (esvalor t) = t
          | otherwise = evalaux t1
           where t1 = eval1p t


-- Reglas de transición
-- eval1p hace una transición mientras se pueda aplicar una regla de transición.
eval1p :: Asa -> Asa
eval1p t = case t of
            (VNum n) -> VNum n
            (VBol b) -> VBol b
            (Var x) -> Var x
            (Suma (VNum n) (VNum m)) -> VNum (n+m)
            (Suma t1 t2@(VNum m)) -> let t1' = eval1p t1 in Suma t1' t2
            (Suma t1@(VNum n) t2) -> let t2' = eval1p t2 in Suma t1 t2'
            (Suma t1 t2) -> Suma (eval1p t1) (eval1p t2)
            (Prod (VNum n) (VNum m)) -> VNum (n*m)
            (Prod t1 t2@(VNum m)) -> let t1' = eval1p t1 in Prod t1' t2
            (Prod t1@(VNum n) t2) -> let t2' = eval1p t2 in Prod t1 t2'
            (Prod t1 t2) -> Prod (eval1p t1) (eval1p t2)
            (Let (Var x) e1 e2) -> eval1p $ sust (eval1p e2) x (eval1p e1)
            (Ifte t1 t2 t3) ->  if esvalor(t1) then 
                                  if t1 == VBol True then eval1p(t2) else eval1p(t3)
                                else
                                  eval1p (Ifte (eval1p t1) t2 t3)
            (Suc (VNum n)) -> VNum (n+1)
            (Suc t) -> Suc(eval1p t)
            (Pred (VNum 0)) -> VNum 0
            (Pred (VNum n)) -> VNum (n-1)
            (Pred t) -> Pred(eval1p t)
            (Iszero(VNum 0)) -> VBol True
            (Iszero(VNum _)) -> VBol False
            (Iszero t) -> Iszero(eval1p t)

-- subst e x r  debe devolver e[x:=r].
-- Let x e1 e2 = e2[x:=e1]


-- 5 Pretty printer
-- Función que transforma un ASA a una expresión en sintaxis concreta. 
concreta :: Asa -> String
concreta (VNum n) = show n
concreta (VBol b) = show b
concreta (Var x) = x
concreta (Suma (VNum n) (VNum m)) = show n ++ " + " ++ show m 
concreta (Suma t (VNum m)) = "("++concreta t ++ ") + " ++ show m 
concreta (Suma (VNum n) t) = show n ++ " + (" ++ concreta t ++ ")"
concreta (Suma t1 t2) =  "("++concreta t1 ++ ") + (" ++ concreta t2 ++ ")"
concreta (Prod (VNum n) (VNum m)) = show n ++ " * " ++ show m 
concreta (Prod t (VNum m)) = "("++concreta t ++ ") * " ++ show m 
concreta (Prod (VNum n) t) = show n ++ " * (" ++ concreta t ++ ")"
concreta (Prod t1 t2) =  "("++concreta t1 ++ ") * (" ++ concreta t2 ++ ")"
concreta (Let (Var x) e1 e2) = "let " ++ x ++ ":=("++ concreta e1 ++ ") in ("++ concreta e2 ++ ")"
concreta (Ifte t1 t2 t3) = "if ("++ concreta t1 ++") then ("++ concreta t2 ++") else ("++ concreta t3 ++")" 
concreta (Suc t) =  "suc"++ "("++ concreta t ++")"
concreta (Pred t) = "pred"++ "("++ concreta t ++")"
concreta (Iszero t) = "isZero("++ concreta t ++ ")"


-- Pruebas
-- Función que recibe el nombre de un archivo que contiene una expresión en sintaxis concreta y la evalúa mostrando el proceso paso a paso.
correPrueba file = do cont <- readFile (file++".eab");       
                      putStr "\n\n";
                      putStr "<<< CONTENIDO DEL ARCHIVO >>>\n";
                      putStr cont;
                      putStr "\n\n";
                      putStr "<<< TOKENS GENERADOS POR EL LEXER >>>\n";
                      let lex = (lexer cont);
                      putStr (show lex);
                      putStr "\n\n";
                      putStr "<<< ASA  GENERADO POR EL PARSER >>>\n";
                      let par = (parse lex);
                      putStr (show par);
                      putStr "\n\n";
                      let tipo = (vt [] par);
                      putStr "<<< TIPO ASA >>>\n";
                      putStr (show tipo);
                      putStr "\n\n";
                      putStr "<<< ASA EVALUADO >>>\n";
                      let eva = (eval par);
                      putStr (show eva);
                      putStr "\n\n";
                      putStr "<<< RESULTADO >>>\n";
                      putStr (concreta (eval (parse (lexer cont))));
                      putStr "\n\n";                         
      

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


--   Sustitución
-- subst e x r  debe devolver e[x:=r].
sust :: Asa -> Ident -> Asa -> Asa
sust = error "Te toca"


--   Valores 
-- Función que nos dice cuándo una expresión es un valor.
esvalor :: Asa -> Bool
esvalor = error "Te toca"


-- Evaluación de expresiones
-- Evalúa las expresiones que están bien tipadas.
eval :: Asa -> Asa
eval = error "Te toca"


-- evalaux hace transiciones mientras no se llegue a un estado final.
evalaux :: Asa -> Asa
evalaux = error "Te toca"


-- Reglas de transición
-- eval1p hace una transición mientras se pueda aplicar una regla de transición.
eval1p :: Asa -> Asa
eval1p = error "Te toca"


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
      

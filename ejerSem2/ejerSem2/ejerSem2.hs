{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}

data Exp = Num Int | Var String | Suma Exp Exp | Let String Exp Exp deriving Show

freeVars::Exp->[String]
freeVars (Num a) = []
freeVars (Var c) = [c]
freeVars (Suma exp1 exp2) = (freeVars exp1) ++ (freeVars exp2)
freeVars (Let x exp1 exp2) = filter (x/=) ((freeVars exp1) ++ (freeVars exp2)) 

sust::Exp->String->Exp->Exp
sust (Num a) varSust exp1 = Num a
sust (Var c) varSust exp1 = if (varSust == c) then exp1 else Var c
sust (Suma exp1 exp2) varSust exp3 = Suma (sust exp1 varSust exp3) (sust exp2 varSust exp3)
sust (Let varSust exp1 exp2) varSust2 exp3 = Let varSust (sust exp1 varSust2 exp3) (sust exp2 varSust2 exp3)

eval::Exp->Int
eval (Num a) = a
eval (Var c) = error "Nope nope nope"
eval (Suma exp1 exp2) = eval(exp1) + eval(exp2)
eval (Let varSust exp1 exp2) = eval (sust exp2 varSust exp1)


{-PRUEBAS-}
-- Debe dar 14.
-- let x = 3 in let y = 7 in x + (4+y) end end
prueba1 = eval $ Let "x" (Num 3) $ Let "y" (Num 7) $ Suma (Var "x") (Suma (Num 4) (Var "y"))

-- Debe dar 20.
-- (let x = (let z = 5 in z+3 end) in x end) + (let y = 7 in 5+y end)
prueba2 = eval $ Suma (Let "x" (Let "z" (Num 5) (Suma (Var "z") (Num 3))) (Var "x")) (Let "y" (Num 7) (Suma (Num 5) (Var "y")))

-- Debe de dar 60.
-- let x = (let y = 3 in (14+y)+5 end) in let z = x+16 in z+x end end
prueba3 = eval $ Let "x" (Let "y" (Num 3) (Suma (Suma (Num 14) (Var "y")) (Num 5))) (Let "z" (Suma (Var "x") (Num 16)) (Suma (Var "z") (Var "x")))

-- Debe de dar 221.
-- let x = 3 + (let y = 98 in 98 + (1+y) end) in let y = 19 in x + (let z = y in z+2 end) end end

--prueba4 =  eval $ Let "x" (Suma (Num 3) (Let "y" (Num 98) (Suma (Num 98) (Suma (Num 1) (Var "y"))))) (Let "y" (Num 19) (Suma (Var "x") (Let "z" (Var "y") (Suma (Var "z") (Num 2)))))
--prueba4 =  eval $ Let "x" (Suma (Num 3) (Let "y" (Num 98) (Suma (Num 98) (Suma (Num 1) (Var "y"))))) (Let "y" (Num 19) (Suma (Var "x") (Let "z" (Var "y") (Suma (Var "z") (Num 2)))))
prueba4 = eval $ Let "x" (Suma (Num 3) (Let "y" (Num 98) (Suma (Num 98) (Suma (Num 1) (Var "y"))))) (Let "y" (Num 19) ( Suma (Var "x") (Let "z" (Var "y") (Suma (Var "z") (Num 2) ))))


{-RETO EXTRA-}
evalreto::Exp->Int->(Int,Int)
evalreto = error "Te toca"
--evalreto (Num a) 0 = (a, 0)
--evalreto (Var c) 0 = error "NOPE"
--evalreto (Suma exp1 exp2) n = (eval(exp1) + eval(exp2), n+1) usar let (v1,n1)
--evalreto (Let varSust exp1 exp2) = eval (sust exp2 varSust exp1)


{-PRUEBAS RETO-}
-- Debe dar (24,3).
prueba1r = evalreto  (Suma (Let "x" (Let "z" (Num 5) $ Suma (Var "z") (Num 3)) $ Var "x") 
                                    (Let "y" (Num 11) $ Suma (Num 5) (Var "y"))) 0 

-- Debe de dar (48,6)
prueba2r = evalreto (Let "x" (Suma (Num 2) (Let "y" (Num 98) $ Suma (Num 98) (Suma (Num 1) (Num 3)))) $
                  Let "y" (Num 19) $ Suma (Suma (Num 8) (Var "y")) (Let "z" (Var "y") $ Suma (Var "z") (Num 2))) 0









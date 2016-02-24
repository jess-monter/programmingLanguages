{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}

data Exp = Num Int | Var String | Suma Exp Exp | Let String Exp Exp deriving Show

freeVars::Exp->[String]
freeVars = error "Te toca"


sust::Exp->String->Exp->Exp
sust = error "Te toca"


eval::Exp->Int
eval = error "Te toca"


{-PRUEBAS-}
-- Debe dar 14.
-- let x = 3 in let y = 7 in x + (4+y) end end
prueba1 = eval $ Let "x" (Num 3) $ Let "y" (Num 7) $ Suma (Var "x") (Suma (Num 4) (Var "y"))

-- Debe dar 20.
-- (let x = (let z = 5 in z+3 end) x end) + (let y = 7 in 5+y end)
prueba2 = error "Te toca escribir la prueba"

-- Debe de dar 60.
-- let x = (let y = 3 in (14+y)+5 end) in let z = x+16 in z+x end end
prueba3 = error "Te toca escribir la prueba"

-- Debe de dar 40.
-- let x = 3 + (let y = 98 in end 98 + (1+y)) in let y = 19 in x + (let z = y in z+2) end end
prueba4 = error "Te toca escribir la prueba"


{-RETO EXTRA-}
evalreto::Exp->Int->(Int,Int)
evalreto = error "Te toca"


{-PRUEBAS RETO-}
-- Debe dar (24,3).
prueba1r = evalreto  (Suma (Let "x" (Let "z" (Num 5) $ Suma (Var "z") (Num 3)) $ Var "x") 
                                    (Let "y" (Num 11) $ Suma (Num 5) (Var "y"))) 0 

-- Debe de dar (48,6)
prueba2r = evalreto (Let "x" (Suma (Num 2) (Let "y" (Num 98) $ Suma (Num 98) (Suma (Num 1) (Num 3)))) $
                  Let "y" (Num 19) $ Suma (Suma (Num 8) (Var "y")) (Let "z" (Var "y") $ Suma (Var "z") (Num 2))) 0









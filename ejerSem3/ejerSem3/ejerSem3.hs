{-
Facultad de Ciencias UNAM
   Lenguajes de programación 2016-2
      Profesor: Noé Salomón Hernández Sánchez
      Ayudante: Albert M. Orozco Camacho
      Ayudante lab: C. Moisés Vázquez Reyes
-}


--Cálculo lambda puro
data LamU = Var String | Lam String LamU | App LamU LamU 


--Para pintar adecuadamente los términos del cálculo lambda
instance Show LamU where
      show e = case e of
                  Var x -> x
                  Lam x t -> "/"++x++"."++show t
                  App (Var x) (Var y) -> x++y 
                  App (Var x) t2 -> x++"("++show t2++")"
                  App t1 (Var y) -> "("++show t1++")"++y
                  App t1 t2 -> "("++show t1 ++") ("++show t2++")"
  
                  
--Para representar sustituciones            
type Sust = (String,LamU)        

--Para calcular las variables libres de un término
fv::LamU->[String]
fv e = case e of
   Var x -> [x]
   Lam x e -> filter (x/=) $ fv e
   App e1 e2 -> fv e1 ++ fv e2 
     
pruebafv = fv $ Lam "x" $ Lam "y" $ App (App (Var "x") (Var "w")) (Var "z")


--Para aplicar una sustitución en el cálculo lambda puro                  
sust::LamU->Sust->LamU
sust e (y,t) = case e of
      Var x -> if x==y then t else Var x
      Lam x w -> if (x `elem` ( y : fv t)) then e else
               Lam x $ sust w (y,t)
      App e1 e2 -> App (sust e1 (y,t)) (sust e2 (y,t))           

pruebaSust = sust (Lam "x" $ Lam "z" $ App (App (Var "x")(Var "y")) (Var "z")) ("y", Var "r")

pruebaSust2 = App suc (Lam "x" $ Lam "z" $ App (Var "s") (Var "z"))

pruebaSustr = sust suc ("n", (Var "r"))
pruebaSustcero = sust suc ("n", (Lam "x" $ Lam "z" $ (Var "z")))
pruebaSust3 = sust suc ("n", (Lam "x" $ Lam "z" $ App (Var "s") (Var "z")))

--Nos dice si hay un redex en un término del cálculo lambda puro            
hayRedex::LamU->Bool
hayRedex e = case e of
         Var _ -> False
         Lam x e -> hayRedex e
         App (Lam x e) (e2) -> True
         App e1 e2 -> if hayRedex e1 || hayRedex e2 then True else False

pruebaRedex = hayRedex $ App (Var "x") (Var "z")

--Realiza la beta-reducción en una expresión
betaR::LamU -> LamU
betaR e = case e of 
      Var x -> Var x
      Lam x z -> if (hayRedex z) then Lam x (betaR z) else Lam x z
      App (Lam x e1) e2 -> sust e1 (x, e2)
      --App e1 (Lam x e2) -> App e1 (Lam x (betaR e2))
      App a b -> if (hayRedex a) then App (betaR a) b else if hayRedex b then App a (betaR b) else
                  App a b
      --App a b -> if (hayRedex b) then App a (betaR b) else if hayRedex a then App (betaR a) b else
      --            App a b
      --App e1 e2 -> if hayRedex e1 then App (betaR e1) e2
      --else if (hayRedex e2) then App (e1) (betaR e2)
      --App (Var x) e2 -> App (Var x) (betaR e2)
      --App e1 (Var x) -> App (betaR e1) (Var x)
--      App e1 (Lam x e2) -> App (betaR e1) (Lam x e2)

pruebaBeta = betaR $ App (Lam "x" $ (Var "x")) (Lam "y" $ (Var "y"))
pruebaBeta1 = fn $ App (Var "x") (App (Lam "z" $ Var "z") (Var "y"))


pruebaBeta2 = fn $ App (App (Lam "x" $ Lam "y" $ App (Var "x") (Var "y")) (Var "z")) (Var "s")

pruebarep = App (App (Lam "x" $ Lam "y" $ App (App (Var "x" ) (Var "y")) (Var "x")) (Var "s")) (Var "z")

--Calcula la forma normal de un término del cálculo lambda puro            
fn::LamU -> LamU
-- fn t = if (hayRedex t) then (betaR t) else t

fn t | not (hayRedex t) = t
     | otherwise = fn t1  
         where t1 = betaR t

pruebafn = fn $ App (Lam "x" $ Var "z") (App (Lam "z" $ Var "z") (Var "y"))
pruebafnBeta = betaR $ betaR $ betaR $ betaR $ App (Lam "x" $ Var "z") (App (Lam "z" $ Var "z") (Var "y"))

--Dado un entero positivo, nos devuelve su representación como numeral de Church
church::Int->LamU
church 0 = Lam "s" $ Lam "z" $ Var "z"
church n = Lam "s" $ Lam "z" $ auxChurch n
                                 where auxChurch 0 = Var "z"
                                       auxChurch n = App (Var "s") (auxChurch (n-1))


--Booleanos en cálculo lambda puro
true = Lam "x" $ Lam "y" $ Var "x"
false = Lam "x" $ Lam "y" $ Var "y"

--Operador if-then-else
ift = Lam "b" $ Lam "t" $ Lam "e" $ App (App (Var "b") (Var "t")) (Var "e")

--Operador iszero para numerales de Church
iszero = Lam "m" $ App (App (Var "m") (Lam "x" $ false)) true

--Operador que construye pares ordenados
pair = Lam "f" $ Lam "s" $ Lam "b" $ App (App (Var "b") (Var "f") ) (Var "s")

--Operador que devuelve la primer componente de un par ordenado
fstU = Lam "p" $ App (Var "p") (true)

--Operador que devuelve el segundo componente de un par ordenado
sndU = Lam "p" $ App (Var "p") (false)

--Sucesor de un numeral de Church
suc = Lam "n" $ Lam "s" $ Lam "z" $ App (Var "s") (App (App (Var "n") (Var "s")) (Var "z"))

pruebaSuc = App (Lam "n" $ Lam "s" $ Lam "z" $ App (Var "s") (App (App (Var "n") (Var "s")) (Var "z"))) (church 1)

pruebaSuc2 = App (Lam "n" $ Lam "s" $ Lam "z" $ App (Var "s") (App (App (Var "n") (Var "s")) (Var "z"))) (Var "r")

shiftInc = Lam "p" $ App (App pair (App sndU (Var "p"))) (App suc (App sndU (Var "p")))

zz = App (App pair (church 0)) (church 0)
ss = Lam "p" $ App (App pair (App sndU (Var "p"))) (App (App suma (church 1)) (App sndU (Var "p")) )

--ss = λp. pair (snd p) (scc (snd p))
--prd = λm. fst (m ss zz)


--Predecesor de un numeral de Church 
predU = Lam "m" $ App fstU (App (App (Var "m") ss) zz)


predU2 = Lam "n" $ Lam "f" $ Lam "x" $ App (App (Var "n") (App (Lam "g" $ Lam "h" $ App (Var "h") (App (Var "g") (Var "f") ) ) (Lam "u" $ Var "x") ) ) (Lam "u" $ Var "u")

predU22 = Lam "n" $ Lam "f" $ Lam "x" $ App (Var "n") (App (App (Lam "g" $ Lam "h" $ App (Var "h") (App (Var "g") (Var "f") ) ) (Lam "u" $ Var "x") ) (Lam "u" $ Var "u"))


predU3 = Lam "n" $ App (fstU) (App (Var "n") (App (shiftInc) (App (App pair (church 0)) (church 0) ) ))
--Suma de naturales de Church 
suma = Lam "n" $ Lam "m" $ App (App (Var "m") (suc)) (Var "n")

--Productor de numerales de Church
--prod = Lam "n" $ Lam "m" $ App (App (Var "m") (suma)) (Var "n")

prod = Lam "n" $ Lam "m" $ Lam "s" $ App (Var "m") (App (Var "n") (Var "s"))

--Operador de punto fijo (Curry-Roser)
pF = Lam "f" $ App (Lam "x" $ App (Var "f") (App (Var "x") (Var "x"))) (Lam "x" $ App (Var "f") (App (Var "x") (Var "x")))


--Nuestra primer función recursiva
fac = App pF g where
               g = Lam "pF" $ Lam "n" $ App (App  (App ift (App iszero (Var "n"))) (church 1) ) (App (App prod (Var "n")) (App (Var "pF") (App (predU) (Var "n") ) ) )
               

factproof = App (Lam "n" $ App (App  (App ift (App iszero (Var "n"))) (church 1) ) (App (App prod (Var "n")) (App predU (Var "n"))) )               

klop = error "Te toca si quieres +5 pts"               
               
   --PRUEBAS

--Debe de dar /s./z.s(s(s(s(s(s(sz))))))
prueba1 = fn $ App (App suma (church 3)) (church 4)
--prueba11 = betaR $ App (App suma (church 3)) (church 4)
--prueba12 = betaR $ betaR $ App (App suma (church 3)) (church 4)
--prueba13 = betaR $ betaR $ betaR $ App (App suma (church 3)) (church 4)
--prueba14 = betaR $ betaR $ betaR $ betaR $ App (App suma (church 3)) (church 4)

--Debe de dar /s./z.s(s(s(s(s(sz)))))
prueba2 = fn $ App (App prod (church 3)) (church 2)

--Debe de dar /s./z.s(s(s(s(s(sz)))))
prueba3 = fn $ App fac (church 3)

--Debe de dar /s./z.s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(sz)))))))))))))))))))))))
prueba4 = fn $ App fac (church 4)

prueba5 = fn $ App fac (church 0)





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
     
--fv $ Lam "x" $ Lam "y" $ App (App (Var "x") (Var "w")) (Var "z")


--Para aplicar una sustitución en el cálculo lambda puro                  
sust::LamU->Sust->LamU
sust e (y,t) = case e of
      Var x -> if x==y then t else e
      Lam x w -> if (elem x $ fv(t)++[y]) then e else
               Lam x $ sust w (y,t)
      App e1 e2 -> App (sust e1 (y,t)) (sust e2 (y,t))           

-- sust (Lam "x" $ Lam "z" $ App (App (Var "x")(Var "y")) (Var "z")) ("y", Var "r")

                   

--Nos dice si hay un redex en un término del cálculo lambda puro            
hayRedex::LamU->Bool
hayRedex e = case e of
         Var _ -> False
         Lam x e -> False
         App (Var x) (Var y) -> False
         App e1 e2 -> if hayRedex e1 || hayRedex e2 then True else False

pruebaRedex = hayRedex $ App (Var "x") (Var "z")

--Dice si la expresion es un valor
esValor:: LamU -> Bool
esValor t = not (hayRedex t)

--Realiza la beta-reducción en una expresión
betaR::LamU -> LamU
betaR e = case e of 
      Var x -> Var x
      Lam x z -> Lam x z
      App (e1) (App (Lam x e2) e3) -> App e1 (betaR (sust e2 (x,betaR e3)))
      App (Lam x e1) e2 -> betaR (sust e1 (x,betaR e2))
      App e1 e2 -> App (betaR e1) (betaR e2)
      --App (Var x) e2 -> App (Var x) (betaR e2)
      --App e1 (Var x) -> App (betaR e1) (Var x)
      --App e1 (Lam x e2) -> App (betaR e1) (Lam x e2)


pruebaBeta1 = betaR $ App (Var "x") (App (Lam "z" $ Var "z") (Var "y"))

pruebaBeta = betaR $ App (Lam "x" $ (Var "x")) (Lam "y" $ (Var "y"))                        

--Calcula la forma normal de un término del cálculo lambda puro            
fn::LamU -> LamU
fn t | esValor t = t
     | otherwise = fn t1  
         where t1 = betaR t

pruebafn = fn $ App (Lam "x" $ Var "z") (App (Lam "z" $ Var "z") (Var "y"))
pruebafnBeta = betaR $ betaR $ betaR $ betaR $ App (Lam "x" $ Var "z") (App (Lam "z" $ Var "z") (Var "y"))

--Dado un entero positivo, nos devuelve su representación como numeral de Church
church::Int->LamU
church 0 = Lam "x" $ Lam "z" $ Var "z"
church n = Lam "x" $ Lam "z" $ auxChurch n
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

--Predecesor de un numeral de Church 
predU = Lam "m" $ App fstU (App (App (Var "m") (Var "s")) (Var "z"))

--Suma de naturales de Church 
suma = Lam "n" $ Lam "m" $ App (App (Var "m") (suc)) (Var "n")

--Productor de numerales de Church
prod = Lam "n" $ Lam "m" $ App (App (Var "m") (suma)) (Var "n")


--Operador de punto fijo (Curry-Roser)
pF = Lam "f" $ App (Lam "x" $ App (Var "f") (App (Var "x") (Var "x"))) (Lam "x" $ App (Var "f") (App (Var "x") (Var "x")))


--Nuestra primer función recursiva
fac = App pF g where
               g = error "Te toca"
               

klop = error "Te toca si quieres +5 pts"               
               
   --PRUEBAS

--Debe de dar /s./z.s(s(s(s(s(s(sz))))))
prueba1 = fn $ App (App suma (church 3)) (church 4)
prueba11 = betaR $ App (App suma (church 3)) (church 4)
prueba12 = betaR $ betaR $ App (App suma (church 3)) (church 4)
prueba13 = betaR $ betaR $ betaR $ App (App suma (church 3)) (church 4)
prueba14 = betaR $ betaR $ betaR $ betaR $ App (App suma (church 3)) (church 4)
--Debe de dar /s./z.s(s(s(s(s(sz)))))
prueba2 = fn $ App (App prod (church 3)) (church 2)

--Debe de dar /s./z.s(s(s(s(s(sz)))))
prueba3 = fn $ App fac (church 3)

--Debe de dar /s./z.s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(sz)))))))))))))))))))))))
prueba4 = fn $ App fac (church 4)





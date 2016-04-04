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
fv = error "Te toca"     
     

--Para aplicar una sustitución en el cálculo lambda puro                  
sust::LamU->Sust->LamU
sust = error "Te toca"                 
                   

--Nos dice si hay un redex en un término del cálculo lambda puro            
hayRedex::LamU->Bool
hayRedex = error "Te toca"


--Realiza la beta-reducción en una expresión
betaR::LamU->LamU
betaR = error "Te toca" 
                        

--Calcula la forma normal de un término del cálculo lambda puro            
fn::LamU->LamU
fn = error "Te toca"


--Dado un entero positivo, nos devuelve su representación como numeral de Church
church::Int->LamU
church = error "Te toca"


--Booleanos en cálculo lambda puro
true = Lam "x" $ Lam "y" $ Var "x"
false = error "Te toca"

--Operador if-then-else
ift = error "Te toca"

--Operador iszero para numerales de Church
iszero = Lam "m" $ App (App (Var "m") (Lam "x" $ false)) true


--Operador que construye pares ordenados
pair = error "Te toca"

--Operador que devuelve la primer componente de un par ordenado
fstU = error "Te toca"

--Operador que devuelve el segundo componente de un par ordenado
sndU = error "Te toca"


--Sucesor de un numeral de Church
suc = Lam "n" $ Lam "s" $ Lam "z" $ App (Var "s") (App (App (Var "n") (Var "s")) (Var "z"))

--Predecesor de un numeral de Church 
predU = error "Te toca"

--Suma de naturales de Church 
suma = error "Te toca"

--Productor de numerales de Church
prod = error "Te toca"


--Operador de punto fijo (Curry-Roser)
pF = Lam "f" $ App (Lam "x" $ App (Var "f") (App (Var "x") (Var "x"))) (Lam "x" $ App (Var "f") (App (Var "x") (Var "x")))


--Nuestra primer función recursiva
fac = App pF g where
               g = error "Te toca"
               

klop = error "Te toca si quieres +5 pts"               
               
   --PRUEBAS

--Debe de dar /s./z.s(s(s(s(s(s(sz))))))
prueba1 = fn $ App (App suma (church 3)) (church 4)

--Debe de dar /s./z.s(s(s(s(s(sz)))))
prueba2 = fn $ App (App prod (church 3)) (church 2)

--Debe de dar /s./z.s(s(s(s(s(sz)))))
prueba3 = fn $ App fac (church 3)

--Debe de dar /s./z.s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(sz)))))))))))))))))))))))
prueba4 = fn $ App fac (church 4)





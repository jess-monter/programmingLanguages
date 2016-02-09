data N = Zero | Suc N deriving Show


-- operador $ -> como parentesis asociando a la derecha

suma :: N -> N -> N
suma Zero n = n
suma (Suc m) n = Suc $ suma n m

prod :: N -> N -> N
prod Zero _ = Zero
prod (Suc m) n = suma (prod m n) n

data DNat = Cero | D DNat | U DNat deriving Show

simplDN :: DNat -> DNat
simplDN  Cero = Cero
simplDN (D Cero) = Cero
simplDN (D n) = f $ D $ simplDN n where
								f (D Cero) = Cero
								f (D n) = D $ simplDN n
simplDN (U n) = U $ simplDN n  --U = 2n+1, D = 2n

toSet :: Eq a => [a] -> [a]
toSet [] = []
toSet (x:xs) = x:(filter (x/=) S toSet xs)

cuantas :: Eq a => a -> [a] -> Int
cuantas _ [] = 0
cuantas x (y:ys) = if x == y then 1+(cuantas x ys)
		else cuantas x ys

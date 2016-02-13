-- Serie de Perrin --

perrin :: Int -> Int
perrin 0 = 3
perrin 1 = 0
perrin 2 = 2
perrin n = perrin(n-2)+perrin(n-3)
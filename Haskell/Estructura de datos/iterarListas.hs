--Funcion que recorre una lista de enteros y le suma 1 a cada elemento
valueOf::Int -> Int
valueOf x = x + 1

listValues :: [Int] -> [Int]
listValues = map (valueOf)



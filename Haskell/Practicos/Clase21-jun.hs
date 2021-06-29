--Practico 6
--Ejercicio 1
data Raiz = Doble Double | Dos Double Double | Compl Double Double
hallarRaiz :: Double -> Double -> Double -> Raiz
hallarRaiz a b c = 
    case b ^2 - 4 * a * c of 
        0 -> Doble (-b / (2 * a))
        n -> if n < 0 
            then Compl undefined undefined
            else Dos undefined undefined

leerDouble :: IO Double
leerDouble = readLn

raices = do 
    a <- leerDouble
    b <- leerDouble
    c <- leerDouble
    case hallarRaiz a b c of
        Doble a -> putStrLn ("Dos raices iguales " ++ show a)
        -- faltan los otros 2 casos

--ahora con bind
raices' = leerDouble >>= (\a -> 
          leerDouble >>= (\b ->
          leerDouble >>= (\c ->
            case hallarRaiz a b c of
                Doble a -> 
                    putStrLn ("Dos raices iguales " ++ show a)
          )))

-- Monada Maybe:
--(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--return a -> Maybe a 
data Val 
    = Number Integer
    | Boolean Bool 
    | Array[Val]           

fromNumber :: Val -> Maybe Integer
fromNumber (Number n) = Just n
fromNumber _ = Nothing

fromArray :: Val -> Maybe [Val]
fromArray (Array n) = Just n
fromArray _ = Nothing

allJust [] = True
allJust (x:xs) = isJust x && allJust xs

fromJust (Just a) = a

sumArrayNumber :: Val -> Maybe Integer
sumArrayNumber (Array a) =
    let nums = map fromNumber a in
    if (allJust nums)
    then Just $ foldr (+) 0 (map fromJust $ map fromNumber a)
    else Nothing
sumArrayNumber _ = Nothing

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

--peero lo anterior se puede hacer mas elegante:
--sequence hace [Maybe a] -> Maybe [a] ::[Maybe a] -> Maybe a
sumArrayNumber' :: Val -> Maybe Integer
sumArrayNumber' (Array a) = do
    l <- sequence (map fromNumber a)
    return $ sum l

--tambien se puede hacer con monada pero sin el sequence
sumArrayNumber'' :: Val -> Maybe Integer
sumArrayNumber'' (Array []) = do
    return 0
sumArrayNumber'' (Array (x:xs)) = do
    x' <- fromNumber x 
    r  <- sumArrayNumber'' (Array xs)
    return (x' + r)
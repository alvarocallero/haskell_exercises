--Ejercicio 8
data OrdPair = OrdPais Double Double

{(x,y) : x < y}


data OrdPair = OrdPais {
    x::Double
    y::Double
}

p12 = Ordpair 1 2

--otra forma de definirlo, el type es un alias nomas
type Ordpair' = (Double,Double)


--Ejercicio 3
and'::Bool -> Bool -> Bool
and' a b = if a then b else a

--ahora el 3 con guardas
andGuardas::Bool -> Bool -> Bool
andGuardas a b 
          | a = b
          | otherwise = False

--ahora con patterm matching
andPM::Bool -> Bool -> Bool
andPM True True = True
andPM _ _ = False


andPM2::Bool -> Bool -> Bool
andPM2 True b = b
andPM2 False _ = false

--ahora usando case
andP'::Bool -> Bool -> Bool
andP' a b = case a of 
            True -> b
            False -> False

--definir el and como infijo
a &&& b = and a b


--Ejercicio 5
edad :: (In, Int, Int) -> (In, Int, Int) -> Int
edad (dn,mn,an) (da,ma,aa) 
    | ma > mn = aa - an
    | ma == mn && da >= dn = aa - an
    | otherwise = aa - an - 1

edad' :: (In, Int, Int) -> (In, Int, Int) -> Int
edad' (dn,mn,an) (da,ma,aa) 
    | ma > mn || ma == mn && da >= dn = aa - an
    | otherwise = aa - an - 1

edad'' :: (In, Int, Int) -> (In, Int, Int) -> Int
edad'' (dn,mn,an) (da,ma,aa) = 
    aa - an - (if ma > mn || ma == mn && da >= dn
               then 0
               else 1)


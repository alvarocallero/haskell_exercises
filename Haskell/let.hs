--Funcion que calcula el area del cilindro unsaod altura y radio
cylinder :: Float -> Float -> Float
cylinder r h =
 let 
    sideArea = 2 * pi * r * h
    topArea = pi * r ^2
 in sideArea + 2 * topArea

(a,b,c) = (1,2,3)

abso::Integer->Integer
abso a 
   | a > 0 = a
   | otherwise = -a

--Una definicion de funcion
f x y = let a = (x + y) /2
        in (a+1)*(a+2)

foo (x:xs) = x ++ x
foo' (x:xs) = x ++ []

a1::(a->b) -> (a -> b)
a1 = undefined

a2::a -> b -> a -> b
a2 = undefined

a3::a -> b ->( a -> b)
a3 = undefined
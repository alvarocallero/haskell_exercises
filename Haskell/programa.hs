num::Integer
num = 3 + 6

square::Integer->Integer
square x = x * x

--La funcoin XOR toma 2 boolenos y devuelve 1 booleano
exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

--La funcion XOR escrita usando pattenr matching
exOr2 :: Bool -> Bool -> Bool
exOr2 True y = not y
exOr2 False y = y

--Funcion que te devuelve el minimo
min :: Integer -> Integer -> Integer
min x y = if x < y then x else y

--Funcion que devuelve el nombre en una tripla
nombre::(String, Int, Int) -> String
nombre(n,e,s) = n

--Funcion que devuelve el primer elemento de una lista
a=[1,2,4]::[Integer]

--Crear una funcion que me devuelva una lista duplicada:
dupList xs = xs ++ xs

--Crear una funcion que determina si una lista esta vacia:
null [] = True
null (_:_) = False

--Funcion que me devuelve la cabeza de la lista:
head (x:_) = x 

--Funcion que me devuelve la cola de la lista:
tail (_:xs) = xs

--Crear una funcion que devuelve los elementos pares
isEven x = x `mod` 2 == 0
evens xs = [x|x<-xs, isEven x, x >= 0]

data Forma = Circulo Float | Rectangulo Float Float
area:: Forma -> Float
area (Circulo r) = 3.14*r*r
area (Rectangulo b a) = b*a
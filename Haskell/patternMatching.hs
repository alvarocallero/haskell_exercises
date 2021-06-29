--Eleguir el 7 de la suerte
lucky :: (Integral a) => a -> String
lucky 7 = "¡El siete de la suerte!"
lucky x = "Lo siento, ¡no es tu día de suerte!"

--Nombrar los numeros del 1 al 5
sayMe :: (Integral a) => a -> String
sayMe 1 = "¡Uno!"
sayMe 2 = "¡Dos!"
sayMe 3 = "¡Tres!"
sayMe 4 = "¡Cuatro!"
sayMe 5 = "¡Cinco!"
sayMe x = "No entre uno 1 y 5"

--Factorial de n
factorial :: Int -> Int
factorial 0 = 1
factorial n = n*factorial(n-1)

--Funcion head casera para listas
head'::[a] -> a
head' [] = error "Es la lista vacia"
head' (x:_) = x

--Función que nos diga algunos de los primeros elementos que contiene una lista
tell :: (Show a) => [a] -> String
tell [] = "La lista está vacía"
tell (x:[]) = "La lista tiene un elemento: " ++ show x
tell (x:y:[]) = "La lista tiene dos elementos: " ++ show x ++ " y " ++ show y
tell (x:y:_) = "La lista es larga. Los primeros dos elementos son: " ++ show x ++ " y " ++ show y

--Funcion recursiva que devuelve el largo de una lista
largoL::[a]->Integer
largoL [] = 0
largoL (_:xs) = 1 + largoL xs

--Funcion recursiva que suma todos los elementos de una lista
sumaL::[Integer] -> Integer
sumaL [] = 0
sumaL (x:xs) = x + sumaL xs

su::[a] -> [a]
su [] = []
su l@(x:xs) = l

--Funcion que devuelve la cola de la lista
tail'::[a] -> [a]
tail' [] = []
tail' (_:xs) = xs
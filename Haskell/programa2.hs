--Funcion que duplica
doubleMe x = x + x + x

--Funcion que que toma dos numeros, los dobla y los suma
doubleUs x y = x*2 + y*2

--Llamar a una funcion usando otra
double x y = doubleMe x + doubleMe y

--Funcion que multiplique un numero por 2 pero solo si ese numero es menor a igual a 100
por2 x = if x < 100 then 
            x*2 
         else 
            x

g :: a -> [a]
g x = ( \x -> x ++ [] ) [x,x]
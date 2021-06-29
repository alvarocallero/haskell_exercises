l = [4,8,15,16,23,42]

--Concatenar 2 listas
l1 = [1,2,3]++[4,5,6]

--Contatenar un elementos con el operador cons
l2 = 'a':" como estas"

--Concatenar elementos en una lista
l3=1:2:3:[]



--lista intensional que reemplace
--cada número impar mayor que diez por “BANG!” y cada número impar menor que diez por “BOOM!”. Si un número no es impar,
--lo dejamos fuera de la lista
bar xs = [if x < 10 then "BOOM" else "BANG"|x<-xs, odd x]

--todos los elementos del 10 al 20 que no fueran 13, 15 ni 19
p=[x|x<-[10..20], x /= 13,x /= 15,x /= 19]

--Devolver el producto de los elementos de 2 listas
productosLista xs xr = [x*y|x<-xs,y<-xr]

--Devolver el producto de los elementos de 2 listas cuyo valor sea mayor que 50
productosLista2 xs xr = [x*y|x<-xs,y<-xr, x*y>50]

--Lista que combine una lista de adjetivos con una lista de nomnbres
nombres = ["Alvaro","Pedro","Mario"]
adjetivos = ["Saltar","Nace","Rie"]

acciones = [x ++ " " ++ y|x<-nombres, y<-adjetivos]

--Version casera de la funcion length
length' xs = sum [1|_<-xs]

--Funcion que devuelve los numeros pares
pares xs = [x|x<-xs, even x]

--Función que dado un nombre y un apellido devuelva sus iniciales.
iniciales::String -> String -> String
iniciales (a:z) (b:s) = [a]++[b]

--Funcion que dada una lista borra todos los elementos menos las letras mayusculas
soloMayusc::[Char]->[Char]
soloMayusc xs = [x|x<-xs, x `elem` ['A'..'Z']]


--Lista de los numeros impares
y=[x|x<-[1..10],x `mod` 2 /= 0]


divisores n = filter (curry div n) [1..100]


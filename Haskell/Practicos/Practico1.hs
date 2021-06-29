--Ejercicio 1
--'mayores' es una func aux que devuelve los 2 numeros mayores de 3 dados
mayores::(Integral a) => a -> a -> a -> (a,a)
mayores x y z = if x < y && x < z then 
                    (y,z)
                else if y < z && y < x then 
                    (x,z) 
                else
                    (x,y)

cuadrado::(Integral a) => a -> a -> a -> a
cuadrado x y z = fst (mayores x y z) * fst (mayores x y z) + snd (mayores x y z) * snd (mayores x y z)




--Ejercicio 2
--Tres enteros positivos cualesquiera pueden servir como longitudes laterales de un triángulo entero, 
--siempre que satisfagan que: el lado más largo es más corto que la suma de los otros dos lados.
--'mayor' devuelve el numero mas grande de entre 3
--'menores' devuelve los 2 numeros menores de 3 dados
menores::(Integral a) => a -> a -> a -> (a,a)
menores x y z = if x > y && x > z then 
                    (y,z)
                else if y > z && y > x then 
                    (x,z) 
                else
                    (x,y)
mayor::(Integral a) => a -> a -> a -> a
mayor x y z = if x > y && x > z then 
                    x
                else if y > z && y > x then 
                    y
                else
                    z
analyze::(Integral a) => a -> a -> a -> Bool
analyze x y z = if mayor x y z <  fst (menores x y z) + snd (menores x y z) then True else False




--Ejercicio 3
--Primero con expresiones condicionales (esta funcion es estricta porque evalua ambas componentes (x e y))
-- NO USAR x == True!!!!! usar if x then y
andCond::Bool -> Bool -> Bool
andCond x y = if x == False && y == False then False
          else if x == False && y == True then False
          else if x == True && y == False then False
          else True

andCondMejorado::Bool -> Bool -> Bool
andCondMejorado x y = if x == False && y == False then False
          else if x == False && y == True then False
          else if x == True && y == False then False
          else True
        
--andPM::Bool -> Bool -> Bool
--andPM False && False = False
--andPM False && True = False
--andPM x y = if x == True && y == False then False else True
--andPM x y = if x == True && y == True then True else False




--Ejercicio 4
implicacion::Bool -> Bool -> Bool
implicacion x y = if x == True && y == False then False else True




--Ejercicio 5
--Formato de la tripla (dia,mes,anio)
--edad(fechaNac)->(fechaActual)
primerElem::(Integer,Integer,Integer) -> Integer
primerElem (a,b,c) = a
segundoElem::(Integer,Integer,Integer) -> Integer
segundoElem (a,b,c) = b
tercerElem::(Integer,Integer,Integer) -> Integer
tercerElem (a,b,c) = c
edad::(Integer,Integer,Integer) -> (Integer,Integer,Integer) -> Integer
edad (a, b, c) (x, y, z) = if primerElem (a, b, c) == primerElem (x, y, z) 
                           && segundoElem (a,b,c) == segundoElem(x, y, z) then 
                           tercerElem(x, y, z) - tercerElem(a, b, c)
                         else
                           tercerElem(x, y, z) - tercerElem(a, b, c) - 1
                           


--Ejercicio 6
--("Juan", 23, 60000) :: (String, Int, Int)
--type Curso = (String, Int, Int)
--let c1 = Curso ("A",1,2)

--Ejercicio 7
--(a)
--let c1 = Curso "Fisica" 123 3
--let e1 = Estudiante "Alvaro" 40347378 2009 [c1]
data Curso = Curso { nombreC :: String
            , codigo :: Int
            , notaAprobacion :: Int
            } deriving (Show)

data Estudiante = Estudiante { nombreE :: String
            , ci :: Int
            , anioIngreso :: Int
            , cursosAprobados :: [Curso]
            } deriving (Show)
            
--(b)
nombreAndCi::Estudiante -> String
nombreAndCi es = nombreE es ++ " " ++ show(ci es)

--(c)
getAnioIngreso::Estudiante -> Int
getAnioIngreso es = anioIngreso es

--(d)
getCodigosCursosAprobados::Estudiante -> Int -> [Int]
getCodigosCursosAprobados xs n = [codigo x|x<-cursosAprobados xs, notaAprobacion x == n]

--(e)
getEstudiantesDelAnio::[Estudiante] -> Int -> [(String, Int)]
getEstudiantesDelAnio xs anio = [(nombreE es, ci es)|es<-xs, anioIngreso es == anio]




--Ejercicio 8
data OrdPair = OrdPair {
    x::Double,
    y::Double
}




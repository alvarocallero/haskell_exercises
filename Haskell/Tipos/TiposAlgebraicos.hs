data Color = Rojo | Amarillo | Azul | Negro | Blanco
data Figura = Circulo Color Float
            | Rectangulo Color Float Float
            | Triangulo Color Float Float Float

--Tipos Algebraicos recursivos
--Lista
data LChar = Nil | Cons Char LChar

--Arboles
data Tree = Empty | Branch Int Tree Tree

--Funcion que sume los elementos de un arbol
sumaTree::Tree -> Int
sumaTree Empty = 0
sumaTree (Branch n lt rt) = n + sumaTree lt + sumaTree rt 

--Funcion que calcule la altura de un arbol
alturaTree::Tree -> Int
alturaTree Empty = 0
alturaTree (Branch n lt rt) = 1 + max (alturaTree lt)(alturaTree rt)

--Funcion para convertir un arbol en una lista
flatten::Tree -> [Int]
flatten Empty = []
flatten (Branch n lt rt) = [n] ++ flatten lt ++ flatten rt


--Expresiones aritmeticas
data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr deriving Show

eval:: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2 

--Manejo de errores
myDiv1 :: Int -> Int -> Int
myDiv1 x y | y /= 0 = div x y


data Empleado = Empleado String Int --Nombre Sueldo
data Usuario = Usuario String String -- Nombre Pass
data Universitario =  Docente String Int -- Nombre Grado
                    | Estudiante String [Int] -- Nombre Cursos aprobados
                    | Egresado String Int -- Nombre Año egreso

class ConNombre a where
    nombre :: a -> String

instance ConNombre Empleado where
    nombre (Empleado n _) = n

instance ConNombre Usuario where
    nombre (Usuario n _) = n

instance ConNombre Universitario where
    nombre (Docente    n _) = n
    nombre (Estudiante n _) = n
    nombre (Egresado   n _) = n


foo x y = if x == y then nombre x else nombre y


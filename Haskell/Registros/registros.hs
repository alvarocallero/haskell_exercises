--Crear un registro de una persona con: nombre, apellidos, edad, altura, número de teléfono y el sabor de su helado favorito
data Person = Person String String Int Float String String deriving (Show)

--Funcion que devuelve el nombre de la persona
nombre::Person -> String
nombre (Person a _ _ _ _ _) = a

--Como hay que escribir una funcion para poder devolver cada campo, usamos los Registros
data Persona = Persona { firstName :: String
            , lastName :: String
            , age :: Int
            , height :: Float
            , phoneNumber :: String
            , flavor :: String
            } deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show, Eq)

type Hola = Car

--Funcion que obtiene toda la informacion de un auto
--tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " " ++  show (y)
--let auto1 = Car "Ford" "Mustang" 1967
--let auto2 = Car "Ford" "Fiesta" 2015

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

--función que toma una figura y devuleva su superficie o área:
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

--Definir un punto en el espacio bidimensional
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

--Crear un tipo con parametros
data Maybe a = Nothing | Just a

--Crear una agenda de telefonos
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

--y luego una funcion que busque si la combinacion de (Name, PhoneNumber) existe
inPhoneBook::Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name phoneNumber phoneBook = (name, phoneNumber) `elem` phoneBook

pb = [("betty","555-2938"),("bonnie","452-2928"),("patsy","493-2928")]

data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq)

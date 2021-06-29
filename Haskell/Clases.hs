--Como esta definida la clase Eq
class Eq' a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

--Luego definimos una instancia de la clase
data TrafficLight = Red | Yellow | Green

instance Eq' TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

--Para poder crear una instancia para Show, implementamos la funcion Show para TrafficLight
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
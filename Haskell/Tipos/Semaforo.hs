
-- Definimos el tipo Semaforo
data Semaforo = Rojo | Verde | Amarillo

--creamos la clase Eq'
{-
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-}
--creamos la instancia para la clase Eq
instance Eq Semaforo where
    Rojo == Rojo = True
    Verde == Verde = True
    Amarillo == Amarillo = True
    _ == _ = False

instance Show Semaforo where
    show Rojo = "Rojo"
    show Verde = "Verde"
    show Amarillo = "Amarillo"
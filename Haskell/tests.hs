a = map repeat b
b = 1 : map (+1) b

divv :: Int -> Int -> Maybe Int
divv x y 
        | y > 0 = Just (x `div` y)
        | otherwise = Nothing

divvv :: Int -> Int -> Either String Int
divvv x y 
        | y > 0 = Right (x `div` y)
        | otherwise = Left "No pibe"

data Empleado = Empleado String Int
data Jugador = Jugador String String

class ConNombre a where
    nombre :: a -> String

instance ConNombre Jugador where
    nombre (Jugador x y) = x

f = id
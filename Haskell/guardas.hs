--implementar nuestro propio compare usando guardas.
myCompare :: (Ord a) => a -> a -> Ordering
myCompare x y
  | x > y = GT
  | x < y = LT
  | otherwise = EQ

--Calcular el IMC
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
 | weight / height ^ 2 <= 18.5 = "Tienes infrapeso ¿Eres emo?"
 | weight / height ^ 2 <= 25.0 = "Supuestamente eres normal... Espero que seas feo."
 | weight / height ^ 2 <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."
 | otherwise = "opaa"

 --Calcular el IMC utilizando una condicion en las guardas, para evitar repertir el calculo weight / height ^ 2
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
 | bmi <= 18.5 = "Tienes infrapeso ¿Eres emo?"
 | bmi <= 25.0 = "Supuestamente eres normal... Espero que seas feo."
 | bmi <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."
 | otherwise = "¡Enhorabuena, eres una ballena!"
 where bmi = weight / height ^ 2

 --Calcular el IMC usando varias condiciones en las guardas
bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
 | bmi <= skinny = "Flaco"
 | bmi <= normal = "Normal"
 | bmi <= fat = "Sobrepeso 1"
 | otherwise = "Sobrepeso 2"
 where 
 bmi = weight / height ^ 2
 normal = 25.0
 fat = 30.0
 skinny = 18.5

 --Obtener las iniciales del nombre y apellido
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
 where 
 (f:_) = firstname
 (l:_) = lastname

-- función que tome una lista de duplas de pesos y estaturas y devuelva una lista de IMCs
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
        where 
        bmi weight height = weight / height ^ 2

comparacion::Int -> String
comparacion x 
            | x > 0 = "Positivo"
            | x == 0 = "Es cero"
            | otherwise = "Negativo"

import Control.Monad ( filterM )
import Control.Monad.Trans.State
--import Control.Monad.State
import Data.Maybe
--sea el siguiente tipo algebraico:  
data Exp = Num Int | Add Exp Exp | Div Exp Exp


--data Maybe a = Just a | Nothing
divM :: Int -> Int -> Maybe Int
a `divM` b = if b == 0 then Nothing
                       else Just (a `div` b)

--sea la siguiente funcion eval
eval :: Exp -> Maybe Int
eval (Num n) = Just n
eval (Add x y) = case eval x of
-- con monadas
-- eval x >>= (\a -> eval y >>= \b -> return (a + b))
                    Nothing -> Nothing
                    Just a -> case eval y of
                            Nothing -> Nothing
                            Just b -> Just (a + b)
eval (Div x y) = case eval x of
                    Nothing -> Nothing
                    Just a -> case eval y of
                        Nothing -> Nothing
                        Just b -> a `divM` b

--Ahora reescribimos lo anterior con Monadas
m >>= f = case m of
            Nothing -> Nothing
            Just x -> f x

--Finalmente obtenemos lo siguiente:
{-
eval' :: Exp -> Maybe Int
eval' (Num n) = return n
eval' (Add x y) = eval' x >>= λa -> eval' y >>= λb -> return (a + b)
eval' (Div x y) = eval' x >>= λa -> eval' y >>= λb -> a `divM` b
-}
--La monada Maybe
{-instance Monad Maybe where
    return = Just
    m>>= k = case m of
                Just x -> k x
                Nothing -> Nothing
    fail _ = Nothing
-}



data Persona = Persona {nombe :: String, edad :: Integer, ci :: String} deriving Show

data Empleado = Empleado String Int --nombre edad
data Jugador = Jugador String String --nombre posicion

class ConNombre a where
    nombre :: a -> String

instance ConNombre Empleado where
    nombre (Empleado x y ) = x

instance ConNombre Jugador where
    nombre (Jugador x y ) = x

main = do 
    a <- return "hola "
    b <- return "que haces"
    putStrLn $ a ++ b

--MONADA sequence -----------------------------------------------------------
x = [Just 1, Just 2, Just 3]
--sequence x == Just [1,2,3]

y = [3,4,5]

--filterM ------------------------------------------------------------------
numbers = [1,2,3,4,5]
pares = filterM (Just . even) numbers --pares = Just [2,4]

--mapM ----------------------------------------------------------------------
sumar = mapM (Just . (+1)) [1,2,3] --Just [2,3,4]

--MONADA DE ESTADO ------------------------------------------------------------
-------------------------------------------------------------------------------
--Definir una funcion numTree que tome un arbol binario con valores en las hojas y retorne uno igual pero con sus
--elementos numerados por una recorrida en orden
data Tree a = Leaf a | Fork (Tree a)(Tree a) deriving Show

numTree:: Tree a -> State Int(Tree (Int,a))
numTree (Leaf a) = do cont <- get
                      put (cont + 1)
                      return $ Leaf (cont,a)
numTree (Fork l r) = do l' <- numTree l
                        r' <- numTree r
                        return $ Fork l' r'


tree = Fork (Leaf 2)(Leaf 3)

--runState (numTree  (Fork (Fork (Leaf 'a') (Leaf 'b')) (Leaf 'c'))) 1(Fork (Fork (Leaf (1,'a')) (Leaf (2,'b'))) (Leaf (3,'c')),4)



--Correspondencia entre el bind y el do notation:
{-
numTree (Fork l r ) = numTree l >>= λa -> numTree r s >>= λb -> λs -> (Fork a b, s)

equivale a:
  do a <- numTree l
     b <- numTree r
     return (Fork a b)


-}

--MONADA READER  --------------------------------------------------------------
-------------------------------------------------------------------------------
addStuff::Int -> Int
addStuff = do
           a <- (*2)
           b <- (+10)
           return (a+b)

--escribimos lo anterior pero mas explicito
addStuff' :: Int -> Int
addStuff' x = let
            a = (*2) x
            b = (+10) x
            in a+b

-- Implementacion de un Stack
type Stack = [Int]

pop :: Stack -> (Int,Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs)

--Implementamos un stack con la monada de estado:
{-
pop' :: State Stack Int
pop' = State $ \(x:xs) -> (x,xs)

push' :: Int -> State Stack ()
push' a = State $ \xs -> ((), a:xs)
-}


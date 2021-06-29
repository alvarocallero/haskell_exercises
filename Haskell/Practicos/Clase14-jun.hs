--Hola mundo
main :: IO ()
main = readInt >>= printInt
--main = putStrLn "Hola mundo!"

--leer un entero desde la entrada standard
readInt :: IO Int
readInt = readLn

--imprimir un entero, () es unit
printInt :: Int -> IO ()
printInt = putStrLn . show

--Practico 6
--Ejercicio 3
readNInts :: Int -> IO [Int]
readNInts 0 = return [] 
readNInts n = readInt         >>= (\k -> 
              readNInts (n-1) >>= (\l ->
              return (k : l)
              ))
-- lo anterior se puede hacer con do notation
readNInts' :: Int -> IO [Int]
readNInts' 0 = return [] 
readNInts' n = do k <- readInt 
                  l <- readNInts' (n-1)
                  return (k : l)

ej3 :: IO ()
ej3 = do n <- readInt
         --controlar n, implementarun validador de n
         l <- readNInts n
         print (maximum l)
              
-- Ejericio 5
--traduccion del do
foo x y = getLine             >>= (\a ->
          putStrLn (a ++ x)   >>
          getLine             >>= (\b ->
          putStrLn (b ++ y)   >>
          putStrLn (a ++ b)))

--Otra funcion util es sequence


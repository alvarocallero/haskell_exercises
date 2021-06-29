--ENTRADA
--Leer strings
getLineImp :: String
getLineImp = undefined

prefijoImp :: String -> String
prefijoImp str = getLineImp ++ str

prefijoIn :: String -> IO String
prefijoIn str = do pre <- getLine
                   return (pre ++ str)

holaChau = do h <- prefijoIn "hola"
              c <- prefijoIn "chau"
              return (h ++ c)

--Leer enteros

--getLine :: IO String
getInt :: IO Int
getInt = do line <- getLine
            return (read line)

suma2 = do x <- getInt
           return (x + 2)


suma2 :: IO Int 




--SALIDA
main = do putStrLn "Hola Mundo"
-- eltipo de main es main::IO()
-- el tipo de putStrLn es putStrLn::String -> IO() 

putStrs []     = return ()
putStrs (x:xs) = do putStrLn $ show x
                    putStrs xs

getInts = do i <- getInt
             if i == 0
                 then return []
                 else do is <- getInts
                         return (i:is)




-- COMBINACION DE ENTRADA Y SALIDA
main2 = do putStrLn "<=="
           is <- getInts
           putStrLn "==>"
           putStrs $ reverse is



-- MANEJO DE ARCHIVOS
type FilePath = String

--readFile   :: FilePath -> IO String
--writeFile  :: FilePath -> String -> IO()
--appendFile :: FilePath -> String -> IO()

-- leer de un archivo
leer = do st <- readFile "Entrada-Salida.hs"
          putStrLn st

-- leer el contenido de un archivo y escribirlo en otro
leImp = do st <- readFile "Entrada-Salida.hs"
           writeFile "Entrada-Salida2.hs" (st ++ "fin")
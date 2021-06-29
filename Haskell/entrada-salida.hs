import Control.Monad

--main = putStrLn "hello world"

main2 = do 
        putStrLn "Hello, what's your name"
        name <- getLine
        putStrLn ("Hey " ++ name ++ " que andaa")


main3 = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = "A " ++ firstName
        bigLastName = "B " ++ lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

--n programa que lee continuamente una línea y muestra esa línea con sus palabras al revés. 
--La ejecución del programa se detendrá cuando encuentre una línea vacía
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

--en Haskell el return lo que hace es convertir un valor puro en una acción IO
--Al utilizar return no causamos que un bloque do termine su ejecución ni nada parecido. Por ejemplo, este programa 
--ejecutará hasta la última línea sin ningún problema
main4 = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line

--leer un carácter hasta que el usuario pulsa la tecla intro:
main5 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()

--La función when toma un valor booleano y una acción IO de forma que si el valor booleano es True, devolverá 
--la misma acción que le suministremos. Sin embargo, si es falso, nos devolverá una acción return ():
--Aquí tienes como podríamos haber escrito el trozo de código anterior que mostraba el uso de getChar utilizando when
main6 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main

main7 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x
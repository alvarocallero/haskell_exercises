--Practico 6
--Ejercicio 1

leerDouble :: IO Double
leerDouble = readLn

main = do
       putStrLn "Ingrese un numero:"
       a <- leerDouble
       putStrLn "Ingrese otro un numero:"
       b <- leerDouble
       putStrLn "Ingrese otro un numero:"
       c <- leerDouble
       let aXc = (read a) * (read c)
       let discr = (read b)^2 - 4 * aXc  
       case signum discr of
           (-1) -> putStrLn("Dos raices complejas diferentes") 
           0    -> putStrLn("Dos raices reales iguales") 
           1    -> putStrLn("Dos raices reales diferentes")
       

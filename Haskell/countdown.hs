--dado un elemento y una lista, devuelve todos los intercambios posibles
interleave :: a -> [a] -> [[a]]
interleave x [ ] = [[x ]]
interleave x (y : ys) = (x : y : ys) : map (y:) (interleave x ys)

--dada una lista, devuelve las permutaciones de ella
perms :: [a] -> [[a]]
perms [ ] = [[ ]]
perms (x : xs) = [zs | ys <- perms xs, zs <- interleave x ys]

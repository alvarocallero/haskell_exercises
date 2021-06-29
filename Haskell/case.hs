--La funcion que teniamos antes de devolver el primer elemento de la lista:
head'::[a] -> a
head' [] = error "Lista vacia"
head' (x:_) = x

--La escribimos usando case
head''::[a] -> a
head'' xs = case xs of
        [] -> error "Lista vacia"
        (x:_) -> x
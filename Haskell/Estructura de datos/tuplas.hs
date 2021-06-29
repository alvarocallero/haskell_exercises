--función que tomara dos vectores 2D (representados con duplas) y que devolviera la suma de ambos
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

--La misma funcion anterior pero mas compacta
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
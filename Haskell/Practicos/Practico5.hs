-- Ejercicio 1
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

-- (a)
--por evaluacion perezosa
head (replicate 5 1)
>
head (1:replicate (5-1) 1)
>
1

--por valor
head (replicate 5 1)
=>
head (1:replicate 4 1)
=>
head (1:1:replicate 3 1)
=>
head (1:1:1:replicate 2 1)
=>
head (1:1:1:1:replicate 1 1)
=>
head (1:1:1:1:1:replicate 0 1)
=>
head (1:1:1:1:1:[])
=*>
head ([1,1,1,1,1])
=>
1

-- (b)
--por evaluacion perezosa
map (∗2) (replicate 2 2)
=>
map (∗2) (2:replicate 1 2)
=>
4:map (*2) (replicate 1 2)
=>
4:map (*2) (2:replicate 0 2)
=>
4:4:map (*2) (replicate 0 2)
=>
4:4:map (*2) ([])
=*>
[4,4]


--por valor
map (∗2) (replicate 2 2)
=>
map (∗2) (2:replicate 1 2)
=>
map (∗2) (2:2:replicate 0 2)
=>
map (∗2) (2:2:[])
=*>
map (∗2) [2,2]
=>
4:map (∗2) [2]
=>
4:4:map (∗2) []
=*>
[4,4]


-- (c)
--por evaluacion perezosa
length (map (*2) (replicate 2 2))
=>
length (map (∗2) (2:replicate 1 2))
=>
length (map (∗2) (2:2:replicate 0 2))
=>
length (map (∗2) (2:2:[]]))
=>
length (map (∗2) [2,2]))
=>
length (4: map (∗2) [2]))
=>
length (4:4: map (∗2) []))
=>
length ([4,4])
=>
2

--por valor
length (map (*2) (replicate 2 2))
=>
length (map (∗2) (2:replicate 1 2))
=>
length (4: map (∗2) (replicate 1 2))
=>
length (4: map (∗2) (2:replicate 0 2))
=>
length (4:4: map (∗2) (replicate 0 2))
=>
length (4:4: map (∗2) ([]))
=>
length ([4,4])
=>
2


--Ejercicio 2
--por evaluacion perezosa
take n (repeat x )
=>
take n (x:repeat x )
=>
take n (x:x:repeat x )
=> (cuando el largo de la lista de x's == n)
[x,x,x,...,x]

--por valor
take n (repeat x )
=>
take n (x:repeat x )
=>
take n (x:x....repeat x )
=>
no termina nunca

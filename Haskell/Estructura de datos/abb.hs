--un árbol es o bien un árbol vacío o bien un elemento que contiene un elemento y otros dos árboles
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

--Vamos a hacer una funcion para insertar en orden en un ABB
--1 función auxiliar para crear un árbol unitario (que solo contiene un elemento)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

--2 función que inserta elementos en un árbol
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) | x == a = Node x left right
                                 | x < a = Node a (treeInsert x left) right
                                 | x > a = Node a left (treeInsert x right)

--Funcion que devuelve true si un elemento x pertenece a un arbol abb y false en caso contrario
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right


--Determinar si dos arboles contienen los mismos valores en las hojas
--y en el mismo orden. Los ´arboles pueden tener formas distintas.
data Btree a = Leaf a | Fork (Btree a) (Btree a)

eqleaves :: Ord a => Btree a -> Btree a -> Bool
eqleaves t t' = leaves t == leaves t'

leaves (Leaf a) = [a]
leaves (Fork l r) = leaves l ++ leaves r

{-
  eqleaves (Fork (Leaf 2) (Leaf 3)) (Leaf 2)
> leaves (Fork (Leaf 2) (Leaf 3)) == leaves (Leaf 2)
> leaves (Leaf 2) ++ (Leaf 3) == leaves (Leaf 2)
> 2 : ([] ++ leaves (Leaf 3)) == 2 : []
> 2 == 2 && ([] ++ leaves (Leaf 3) == [])
> leaves (Leaf 3) == []
> 3 : [] == []
> False
-}

{-
ones = 1 : ones

   head (tail ones)
>  head (tail (1 : ones)))
>  head (ones)
>  head (1 : ones)
>  1
-}
--Arbol que es o bien vacio, o bien un elemento que contiene un elemento y otros 2 arboles
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Funcion que inserta un elemento de forma ordenada
treeInsert::(Ord a ) => a -> Tree a -> Tree a
treeInsert x EmptyTree = createTree x
treeInsert x (Node a left right) 
                        | x < a = Node a (treeInsert x left) right
                        | x > a = Node a left (treeInsert x right)  
                        | x == a = Node a left right


-- vamos a usar una funcion auxiliar para crear un arbol unitario (con 1 solo elemento)
createTree::a -> Tree a
createTree x = Node x EmptyTree EmptyTree

-- Funcion para determinar si un elemento pertenece a un arbol
elemTree::(Ord a ) => a -> Tree a -> Bool
elemTree x EmptyTree = False
elemTree x (Node a left right) 
                        | x < a = elemTree x left
                        | x > a = elemTree x right  
                        | x == a = True
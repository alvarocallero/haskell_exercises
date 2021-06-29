{- Grupo: X
   Integrante(s):
     Apellido, Nombre, XXXXXXXX
     Apellido, Nombre, XXXXXXXX
-}
module Examples where

import JSONLibrary
import TypedJSON

instance FromJSON Integer where
  fromJSON = undefined
instance ToJSON Integer where
  toJSON   = undefined

instance FromJSON Bool where
  fromJSON = undefined
instance ToJSON Bool where
  toJSON   = undefined

instance FromJSON a => FromJSON [a] where
  fromJSON = undefined
instance ToJSON a => ToJSON [a] where
  toJSON = undefined

instance FromJSON () where
  fromJSON = undefined
instance ToJSON () where
  toJSON   = undefined


data BinTree a
    = Fork {l :: BinTree a, r :: BinTree a}
    | Leaf {val :: a}
    deriving (Show, Eq)

{-
La notación anterior es equivalente a definir:

data BinTree a
    = Fork (BinTree a)(BinTree a)
    | Leaf a
    deriving (Show, Eq)

l :: BinTree a -> BinTree a
l (Fork l _) = l

r :: BinTree a -> BinTree a
r (Fork _ r) = r

val :: BinTree a -> BinTree a
val (Leaf a) = a

-}

instance (FromJSON a) => FromJSON (BinTree a) where
     fromJSON = undefined

instance ToJSON a => ToJSON (BinTree a) where
    toJSON = undefined

-- ejemplo cursos

-- decide si un valor que representa un estudiante esta bien formado
estBienFormado :: JSON -> Bool  
estBienFormado a = undefined

-- dado un valor JSON que representa a un estudiante, retorna su cédula
ci :: JSON -> Maybe String
ci = undefined

-- obtiene arreglo con cursos que fueron aprobados
aprobados :: JSON -> Maybe JSON
aprobados = undefined

-- obtiene arreglo con cursos rendidos en un año dado
enAnio :: Integer -> JSON -> Maybe JSON
enAnio = undefined

-- retorna el promedio de las notas de los cursos
promedioEscolaridad :: JSON -> Maybe Float
promedioEscolaridad = undefined 

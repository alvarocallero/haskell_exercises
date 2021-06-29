--Ejercicio 5
data HTree a =  Bin (HTree a) a (HTree a)  | Tip a --ára que los constuctores queden en el mismo orden que el fold

texample = Bin (Tip (3::Int)) 5 (Bin (Tip 3) 5 (Tip 4))

mapHT :: (a -> b) -> (HTree a -> HTree b)
mapHT f (Tip a) = Tip (f a)
mapHT f (Bin l a r) = Bin (mapHT f l) (f a) (mapHT f r)

--map para las listas (no se pedia)
map' f [] = []
map' f (a:l) = f a : map' f l

--no se pide pero hacemos el fold para mapHT
foldHT :: (b -> a -> b -> b) -> (a -> b) -> HTree a -> b
foldHT f g (Bin l a r) = f (foldHT f g l) a (foldHT f g r)
foldHT f g (Tip a) = g a

--mapHT'
mapHT' :: (a -> b) -> HTree a -> HTree b
mapHT' f = foldHT h g
    where
        h l a r = Bin l (f a) r 
        g a = Tip (f a)

sumTree = foldHT h g
    where 
        h l a r = l + a + r
        g a = a


-- Ejercicio 6

-- a y b
class Sizeable a where
    size :: a -> Int

instance Sizeable Char where
    size _ = 1

instance Sizeable Int where
    size a = abs a

instance (Sizeable a, Sizeable b) => Sizeable (a,b) where
    size (a,b) = size a + size b

instance Sizeable a => Sizeable [a] where
    size = sum . map size

-- e
isSmaller a b = size a < size b

-- f
class (Sizeable a) => Enumerate a where
    enum :: Int -> [a]

instance Enumerate Int where
    enum n = [-n..n] 
    -- enum n = [1..n] ++ map (\a -> a) [1..n]++ [0]

instance (Enumerate a, Enumerate b) => Enumerate (a,b) where
    enum n = [(a,b) | a <- enum n, b<- enum n , size a + size b <= n]

--Ej 3
mkTree [] = Empty 
mkTree (x:xs ) = insert x $ mkTree xs
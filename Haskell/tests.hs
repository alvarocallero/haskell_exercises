data Foo a = Foo a [Foo a ]
foo e (Foo x y) = x : foldl foo e y



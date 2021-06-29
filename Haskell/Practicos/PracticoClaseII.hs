--Ejercicio 
--(a)
--type Estudiante = (String, Int, Int, [(String, Int, Int)])

--getNomCi::Estudiante -> (String, Int)

type NomEst = String
type CI = Int
type Ano = Int
type Curso = (NomCur, CodCurso, Nota)

type NomCur = String
type CodCurso = Int
type Nota = Int
type Estudiante = (NomEst, CI, Ano, cursosAna)

cursosAna = [("CDIV",1061,4),("CDVV",1062,5),("GAL1",1030,4),("P1",1322,12)]
ana::Estudiante
ana = ("Ana", 12345678::Int, 2018, cursosAna) 
  where cursosAna = [("CDIV",1061,4),("CDVV",1062,5)]

  getNomCi::Estudiante -> (NomEst, CI)
  getNomCi (nom, ci, _, _) = (nom,ci)

--(d)
getCursosNota::Estudiante -> Nota -> [CodCurso]
getCursosNota(_,_,_, cursos) n = [cod | (_,cod,nota) <- cursos, nota==n]


--Ejercicio 10
data Rat = Rat Int Int deriving Show

mkRat::Int -> Int -> Rat
--mkRat a b = Rat (a `div` g) (b `div` g)
 -- where g = gcd a b
 mkRat a b = Rat (a `div` (gcd a b))
                 (b `div` (gcd a b))

--otra forma
mkRat::Int -> Int -> Rat
mkRat a b = let g = gcd a b
            in Rat (a `div` (gcd a b))
                 (b `div` (gcd a b)) 

--ahora ve rel tema de los signos, uno mayor a otro
mkRat::Int -> Int -> Rat
mkRat a b 
    | b > 0 = Rat (a `div` g)
                  (b `div` g)
    | otherwise = Rat (-a `div` g)
                      (-b `div` g)
     where g = gcd a b

ratProd::Rat->Rat->Rat
ratProd (Rat a b)(Rat c d) = mkRat (a*c) (b*d)

--obliga a usar un cierto constructor
module Rat (mkRat, Rat(..)) where
f:: Int -> Int -> Int -> Int
f m n p 
    | m >= n && m >= p = m
    | n >= m && n >= p = n
    | otherwise        = p 


{-
  f (2+3) (4-1) (3+9)
> (2+3) >= (4-1) && (2+3) >= (3+9)
> 5 >= 3 && 5 >= (3+9)
> 5 >= (3+9)
> 5 >= 12
> False 
> (4-1) >= (2+3) && (4-1) >= (3+9)
> 3 >= 5 && 3 >= 12
> False && 3 >= 12
> False
> (3+9)
> 12
-}
sumFourthPowers n = sum (map (^4) [1..n])
ffl p = foldl (\ys x -> if p x then x : ys else ys) [ ] 
--reescribir la siguiente expresion usando $:
a = sum (filter (>10) (map (*2) [2..10]))
b = sum $ filter (>10) $ map (*2) $ [2..10]
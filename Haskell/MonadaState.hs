import Control.Monad

type Stack = [Int]

{-
stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    stackNow <- get
    if stackNow == [1,2,3]
    then put [8,3,1]
    else put [9,2,1]
-}
main :: IO ()
main = do
  s <- liftM2 (+) readLn readLn
  print s
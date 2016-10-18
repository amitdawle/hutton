module Main where
import Chapter9

main :: IO ()
main = do 
     --print (length $ solutions' [1,3,7,10,25,50] 765)
      -- print (length $ solutions [1,3,7,10,25,50] 765)
      -- print (closestSolutions' [1,3,7,10,25,50] 831)
       print(length $ concatMap (Chapter9.eval) $  concatMap (exprs) $ choices [1,3,7,10,25,50])
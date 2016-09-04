module Chapter1 where

myproduct [] = 1
myproduct (x:xs) = x * myproduct xs


qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++  qsort larger
             where smaller = qsort [a | a <- xs , a <= x ]
                   larger = qsort [b | b <- xs , b > x]


revQsort :: (Ord a) => [a] -> [a]
revQsort [] = []
revQsort (x:xs) = revQsort as ++ [x] ++  revQsort bs
             where as = qsort [a | a <- xs , a > x ]
                   bs = qsort [b | b <- xs , b <= x]

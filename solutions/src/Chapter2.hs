module Chapter2 where

-- 3
n  = a `div` (length xs)
       where a = 10
             xs = [1,2,3,4,5]


last' xs = head ( drop (length xs  - 1) xs)             

last''  = head.reverse

init' = reverse . tail . reverse

init'' xs = take (length xs - 1) xs 
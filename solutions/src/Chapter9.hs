module Chapter9 where
import Data.List
import Data.Function


data Op = Add | Sub | Mul | Div | Exp


instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"


valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = x /= 1 && y /= 1 && y > 0 && x `mod` y == 0
valid Exp x y = x /= 1 && y /= 1 && y > 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
   show (Val x) = show x   
   show (App o l r) = "(" ++ (show l) ++ " " ++ show o ++ " " ++ (show r) ++ ")" 


values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r


eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l , y <- eval r , valid o x y ]


powerSet :: [Int] ->  [[Int]]
powerSet [] = [[]]
powerSet (x:xs) = map (x:) yss  ++ yss 
                 where yss = powerSet xs 


choices :: [Int] -> [[Int]]
choices xs = concatMap  permutations (powerSet xs)


solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =  elem (values e) (choices ns) && eval e == [n]



split :: [a] -> [([a], [a])]
split [] = []
split [x] = []
split (x:xs) = ([x], xs) : [ (x:l , r)  | (l, r) <- split xs  ]


ops = [Add, Sub, Div, Mul, Exp]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs xs = [ App o l r | (ls, rs) <- split xs, l <- exprs ls,  r <- exprs rs, o <- ops ]


solutions :: [Int] -> Int -> [Expr]
solutions xs n = [ e | c <- choices xs , e <- exprs c, eval e == [n]]



solutionsUsingAllTerms :: [Int] -> Int -> [Expr]
solutionsUsingAllTerms [] _ = []
solutionsUsingAllTerms ns n = [ e | p <- permutations ns, e <- exprs p, eval e == [n]]
                           

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n)]
results ns = [ c | (ls, rs) <- split ns, l <- results ls, r <- results rs, c <- combine'  l r ]


combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [ (App o l r, apply o x y) | o <- ops , valid o x y ]


solutions' :: [Int] -> Int -> [Expr]
solutions' xs n = [ e | c <- choices xs , (e, v) <- results c, v == n]


---1
choices2 :: [Int] -> [[Int]]
choices2 xs =  [ p | s <- powerSet xs, p <- permutations s ]


isChoice :: Eq a => [a] ->[a] -> Bool
isChoice xs [] = False
isChoice [] ys = True
isChoice (x:xs) ys = elem x ys && isChoice xs (remove x ys) 


remove :: (Eq a) => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) 
         | x == y = ys 
         | otherwise = y : ( remove x ys )



closestSolutions' :: [Int] -> Int -> Result
closestSolutions' xs n =   minimumBy (compare `on` snd) $ [ (e, abs (v - n)) | c <- choices xs , (e, v) <- results c]

orderedBySimplicity :: [Int] -> Int -> [Expr]
orderedBySimplicity xs n = sortBy ( compare `on` expressionWeight ) ( solutions' xs n)


expressionWeight :: Expr -> Int
expressionWeight (Val _) = 0
expressionWeight (App Add l r) = 1 + (expressionWeight l) + (expressionWeight r)
expressionWeight (App Sub l r) = 1 + (expressionWeight l) + (expressionWeight r)
expressionWeight (App Mul l r) = 2 + (expressionWeight l) + (expressionWeight r)
expressionWeight (App Div l r) = 3 + (expressionWeight l) + (expressionWeight r)
expressionWeight (App Exp l r) = 4 + (expressionWeight l) + (expressionWeight r)








    
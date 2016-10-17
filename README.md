# Solutions for Programming in Haskell, Second Edition

My solutions to the exercises from the book https://www.amazon.com/Programming-Haskell-Graham-Hutton/dp/1316626229.

#### Chapter 1 

1. Possible calculation for result of double
 
  ```
    double (double 2)
    =  {applying inner double}
    double ( 2 + 2 )
    = {applying outer double }
    (2 + 2) + ( 2 + 2)
    = {applying first +}
      4 + (2 + 2)
    = {applying second (inner) +}
     4 + (4)
    = {applying +}
    8
  ```
2. Show sum[x] = x
  ```
   -- From the definition of sum[x]on page 5
   sum[x]
    = {applying sum}
     x + (sum [])
    = {applying sum}
     x + 0
    = {applying +}
    x
   
  ```
3. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter1.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter1Spec.hs).
4. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter1.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter1Spec.hs).
5. Replacing <= with < in qsort will eliminate duplicates and will lead to an incorrect result
  ```
  qsort1 [2,2,3,1,1] => [1,2,3]
  ```

#### Chapter 2
1. To be done in GHCi
2. Parenthesise
  ```  
    2 ^ 3 * 4 => (2^3) * 4
    Prelude> 2^3*4 
    32
  
    2 * 3 + 4 * 5 => (2 * 3) + ( 4 * 5 )
    Prelude> 2 * 3 + 4 * 5 
    26
  
    2 + 3 * 4 ^ 5 => 2 + (3 * (4 ^ 5))
    Prelude> 2 + 3 * 4 ^ 5
    3074
    ```
3. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter2.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter2Spec.hs).

4. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter2.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter2Spec.hs).

5. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter2.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter2Spec.hs). One version uses tail and reverse and behaves slightly differently than the standard init. It does not throw exception for empty list. 


#### Chapter 3
1. See Appendix A.
2. See Appendix A.
3. Types 
  ```
  second :: [a] -> a
  second xs = head (tail xs)
  
  swap :: (a,b) -> (b,a)
  swap (x,y) = (y,x)
  
  pair :: a -> b -> (a,b)
  pair x y = (x,y)
  
  double ::(Num a) => a -> a 
  double x = x * 2
  
  palindrome :: (Eq a) => [a] -> Bool
  palindrome xs = reverse xs == xs
  
  twice :: (a -> a) -> a -> a   --  f cannot be a -> b otherwise f (f x) will fail with type error
  twice f x = f (f x)
  ```
4. Verify types above are correct (they are)
5. In general we do not have Eq for function types as this would mean we need to define instances for every possible function type. Also to define Eq, we may have to test the entire domain for the functions to see if they map to exactly the same values in the co-domain.

(5b) Consider two functions
```
f1 x = x + x
f2 x = 2 * x 
```  
Both the functions produce same output for same input even though they are defined differently. f1 and f2 are effectively equal and we can substitute one for another.
  
#### Chapter 4
1. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter4.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter4Spec.hs).
2. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter4.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter4Spec.hs).
3. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter4.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter4Spec.hs).
4. For (||)
  ```
  True || True = True
  True || False = True
  False || True = True
  False || False = False
  
  ---- Same as
  False || False = False
  _ || _ = True
 ```
5. Meaning of pattern matching definition 
  ```
  True && True = True
  _ && _ = False
  
  -- With conditionals
  
  if (a) then (if (b) then b else False) else False
  ```
6.  Meaning of pattern matching definition 
  ```
  True && b = b
  False && _ = False
  
  -- With conditionals
  
  if (a) then b else False
  ```
7. Curried function as lambda

    ```
     mult :: Int -> Int -> Int -> Int
     mult x y z = x * y * z
     mult = \x -> (\y -> (\z -> x * y * z ))
    ``` 
 
#### Chapter 5
1. Sum of squares
 
  ```
  sum [ x^2 | x<- [1..100]]
  -- or
  sum . map (^2) $  [1..100]
  ```
2. Grid
 
  ```
   Prelude> [(x,y)| x <-[0..1], y<-[0..2] ]
   [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
  ```
3. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter5.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter5Spec.hs).
4. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter5.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter5Spec.hs).
5. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter5.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter5Spec.hs).
6. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter5.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter5Spec.hs).
7. Single Generator

   ```
    [(x,y) | x<-[1,2], y<-[3,4]]
    >> [(1,3),(1,4),(2,3),(2,4)]
    -- Or with concat and nested generators
    concat [ [(x,y) | y <- [3,4]]  |  x <- [1,2] ]
    >> [(1,3),(1,4),(2,3),(2,4)]
   ``` 
8. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter5.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter5Spec.hs). 


#### Chapter 6

All. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter6.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter6Spec.hs). 

#### Chapter 7

All. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter7.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter7Spec.hs).

Ex 9 is tested as part of ex 10.

#### Chapter 8

All. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter8.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter8Spec.hs).

Exercise 7. As Eq instance Maybe and [] is already in defined, the solution is commented out.


#### Chapter 9

All. See [src](https://github.com/amitdawle/hutton/blob/master/solutions/src/Chapter9.hs) and the corresponding [test](https://github.com/amitdawle/hutton/blob/master/solutions/test/Chapter9Spec.hs).



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
  2 ^ 3 * 4 => (2^3) * 4
  ```
  Prelude> 2^3*4 
  32
  ```
  2 * 3 + 4 * 5 => (2 * 3) + ( 4 * 5 )
  ```
  Prelude> 2 * 3 + 4 * 5 
  26
  ```
  2 + 3 * 4 ^ 5 => 2 + (3 * (4 ^ 5))
  ```
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
  


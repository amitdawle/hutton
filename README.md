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

3. See source and the corresponding test.

4. See source and the corresponding test.

5. Replacing <= with < in qsort will eliminate duplicates and will lead to an incorrect result
```
qsort1 [2,2,3,1,1] => [1,2,3]
```

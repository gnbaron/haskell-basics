length [] = 0
length (_:c) = 1 + length c

selectionsort [] = []
selectionsort lst = x : selectionsort (remove x lst)
          where x = lower lst 

lower [a] = a
lower (a:b) = min a (lower b)

removeTodos a lst = [x | x <- lst, x /= a]

removeTodos2 a lst = filter (/=a) lst

remove :: Ord a => a -> [a] -> [a] 
remove _ [] = []
remove a (h:t) = if a == h then t else h : remove a t  

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:r) = quicksort lowers ++ [p] ++ quicksort highers
    where
        lowers  = [ y | y <- r, y < p ]
        highers = [ y | y <- r, y >= p ]
    

f :: Double -> Double
f x = let triple = x * x * x
      in triple * 2 + 2 * triple + 1 / triple
    
    
fib :: Int -> Int
fib x | x == 1 = 0
      | x == 2 = 1
    | otherwise = fib (x-1) + fib (x-2)

(!=) :: Int -> Int -> Bool
a != b = a /= b

(+*) :: Char -> Int -> String
c +* 0 = ""
c +* x = c : (c +* (x-1))

infix 4 !=
infixl 8 +*

mergesort []  = []
mergesort [x] = [x]
mergesort lst = beat (mergesort m1) (mergesort m2)
        where 
          half = div (length lst) 2
          m1 = take half lst
          m2 = drop half lst

beat [] lst = lst
beat lst [] = lst
beat (p1:r1) (p2:r2)
  | p1 < p2   = p1 : beat r1 (p2:r2)
  | otherwise = p2 : beat (p1:r1) r2 

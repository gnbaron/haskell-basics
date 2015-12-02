--List contains
contains e [] = False
contains e (h:t) = if(h == e) then True else contains e t
--result = contains 5 [1,9]
--result = contains 5 [1,23,4,5]

--Invert list
invert [] = []
invert (h:t) = invert t ++ [h]
--result = invert [1,2,3,4,5,6]

-- Sum elements in same position [5,2] [2,3] = [7,5]
sumList [] [] = []
sumList (h1:t1) (h2:t2) = [h1+h2] ++ sumList t1 t2
--result = sumList [5,2] [2,3]

-- Palindrome
palindrome word = word == invert word
--result = palindrome ["a","b","u","b","a"]

--Pair values
--testing first class functions
pairs_s numbers = map (\n -> if (mod n 2) == 0 then "pair" else "odd") numbers
--result = pairs_s [0,1,2,3,4,5,6,7,8,9,10]

pairs numbers = filter (\n -> (mod n 2) == 0) numbers
result = pairs [0,1,2,3,4,5,6,7,8,9,10]

main = do putStrLn(show(result))

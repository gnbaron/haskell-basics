--1 
contains e [] = False
contains e (h:t) = if(h == e) then True else contains e t

main = do putStrLn(show(contains 5 [1,23,4,5]))

let plus :: Int -> Int -> Int = \x -> \y -> x in ((\f -> plus f (f 1)) :: Bot -> Int)

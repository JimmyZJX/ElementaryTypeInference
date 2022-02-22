let g :: (forall a. Int -> a -> Int) -> Bool -> Int = \k -> k @Bool 3 in g

let f :: (forall a. Int -> a -> Int) -> Bool -> Int = \k -> k 3 in f

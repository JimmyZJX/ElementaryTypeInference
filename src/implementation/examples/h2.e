let g :: (forall a. Int -> a -> Int) -> Bool -> Int = \k -> k 3 in
	let h2 :: (forall b. forall a. b -> a -> b) -> Bool -> Int = \k -> g k in h2

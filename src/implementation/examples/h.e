let f :: (forall a. Int -> a -> Int) -> Bool -> Int = \k -> k 3 in
	let h :: (forall b. forall a. b -> a -> b) -> Bool -> Int = \k -> f k in h

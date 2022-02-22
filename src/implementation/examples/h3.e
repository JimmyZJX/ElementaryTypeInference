let g :: (forall a. Int -> a -> Int) -> Bool -> Int = \k -> k @Bool 3 in
	let h3 :: (forall b. forall a. b -> a -> b) -> Bool -> Int = \k -> g k in h2

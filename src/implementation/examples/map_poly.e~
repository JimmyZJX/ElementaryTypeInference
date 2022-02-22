let map :: forall a. forall b. (a -> b) -> [a] -> [b] =
    /\ a. /\ b. \f -> \xs -> case xs of
                                 [] -> [];
                                 (y : ys) -> f y : map f ys
    in map @Int @Top (\x -> x :: Top) (1 : 2 : 3 : [])

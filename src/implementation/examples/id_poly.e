let id :: forall a. a -> a =
    \ x -> x 
    in id @(forall a. a -> a) id

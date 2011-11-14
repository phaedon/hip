-- | Basic implementation of lifting for functions.
module Hip.Lift (lift1, lift2, lift3) where

-- | Unary lifting
lift1 :: (a -> b) 
         -> (f -> a) -> f -> b
lift1 op img pt = op (img pt)

-- | Binary lifting
lift2 :: (a -> b -> c) 
         -> (f -> a) -> (f -> b) -> f -> c
lift2 op img1 img2 pt = op (img1 pt) (img2 pt)

-- | Ternary lifting
lift3 :: (a -> b -> c -> d) 
         -> (p -> a) -> (p -> b) -> (p -> c) -> p -> d
lift3 h f1 f2 f3 p = h (f1 p) (f2 p) (f3 p)




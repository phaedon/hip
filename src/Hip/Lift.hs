module Hip.Lift where

lift0 :: a -> f -> a
lift0 h _ = h

lift1 :: (a -> b) -> (f -> a) -> f -> b
lift1 h f1 p = h (f1 p)

lift2 :: (a -> b -> c) -> (f -> a) -> (f -> b) -> f -> c
lift2 h f1 f2 p = h (f1 p) (f2 p)

lift3 :: (a -> b -> c -> d) -> (p -> a) -> (p -> b) -> (p -> c) -> p -> d
lift3 h f1 f2 f3 p = h (f1 p) (f2 p) (f3 p)




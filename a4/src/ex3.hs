xor :: [Bool] -> Bool
xor xs = odd $ foldr (\x y -> if x then 1 + y else y) 0 xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (*) 1 . fmap (flip (-) 2) . filter even $ xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n
  | even n = sum $ takeWhile (>1) $ iterate f n
  | otherwise = fun2' (3 * n + 1)
  where f x | even x = x `div` 2
            | otherwise = (3 * x + 1)

ff x | even x = x `div` 2
     | otherwise = (3 * x + 1)


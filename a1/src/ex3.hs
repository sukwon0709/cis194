sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ fmap (sum . toDigits) xs

toDigits :: Integer -> [Integer]
toDigits n
  | n < 0 || n == 0 = []
  | n < 10 = [n]
  | otherwise = toDigits (n `div` 10) ++ toDigits (n `mod` 10)

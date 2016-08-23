toDigits :: Integer -> [Integer]
toDigits n
  | n == 0 = []
  | otherwise = toDigits' n
  where toDigits' n
          | n < 0 = []
          | n < 10 = [n]
          | otherwise = toDigits' (n `div` 10) ++
                        toDigits' (n `mod` 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

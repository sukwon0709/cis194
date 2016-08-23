validate :: Integer -> Bool
validate n =
  let digitSum = sumDigits . doubleEveryOther . toDigits $ n
  in digitSum `mod` 10 == 0

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ fmap (sum . toDigits) xs

toDigits :: Integer -> [Integer]
toDigits n
  | n == 0 = []
  | otherwise = toDigits' n
  where toDigits' n
          | n < 0 = []
          | n < 10 = [n]
          | otherwise = toDigits' (n `div` 10) ++
                        toDigits' (n `mod` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther xs = reverse . doubleNext . reverse $ xs
  where doubleNext []     = []
        doubleNext [x]    = [x]
        doubleNext (x:xs) = x : doubleNow xs
        doubleNow []     = []
        doubleNow [x]    = [x*2]
        doubleNow (x:xs) = (x*2) : doubleNext xs

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

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let pairs = filter (\(x,y) -> x <= y) $ cartProd [1..n] [1..n]
      pairsToRemove = filter (\(x,y) -> x + y + 2*x*y <= n) pairs
      toRemove = fmap (\(x,y) -> x+y+2*x*y) pairsToRemove
      removed = filter (\x -> x `notElem` toRemove) [1..n]
  in fmap (+1) $ fmap (*2) removed

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

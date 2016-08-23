type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c d
  | n == 1 = [(a, b)]
  | n == 2 = [(a, c)] ++ [(a, b)] ++ [(c, b)]
  | otherwise = hanoi (n-2) a c b d ++
                [(a, d)] ++
                [(a, b)] ++
                [(d, b)] ++
                hanoi (n-2) c b a d

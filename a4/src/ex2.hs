data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = foldr f Leaf xs
  where f x Leaf                  = Node 0 Leaf x Leaf
        f x (Node h left n right)
          | getHeight left < getHeight right = (Node h (f x left) n right)
          | getHeight left > getHeight right = (Node h left n (f x right))
          | otherwise = (Node (h+1) (f x left) n right)

getHeight :: Tree a -> Integer
getHeight Leaf           = 0
getHeight (Node h _ _ _) = h

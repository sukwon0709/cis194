module Golf where

import           Data.List


skips :: [a] -> [[a]]
skips xs = skips' xs 1
  where skips' l n
          | length l >= n = everyN n l : skips' l (n+1)
          | otherwise = []

everyN :: Int -> [a] -> [a]
everyN n xs =
  let grouped = groupN n xs
  in concat $ fmap (drop (n-1)) grouped

groupN :: Int -> [a] -> [[a]]
groupN n xs
  | length xs <= n = [xs]
  | otherwise = let (f, s) = splitAt n xs
                in f : groupN n s


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = if y > x && y > z
                         then y : localMaxima (y:z:xs)
                         else localMaxima (y:z:xs)
localMaxima others = []


histogram :: [Integer] -> String
histogram xs =
  let rows = histogram' (group . sort $ xs)
  in unlines (reverse rows) ++ "==========\n0123456789\n"

histogram' :: [[Integer]] -> [String]
histogram' xxs
  | concat xxs == [] = []
  | otherwise =
    let curRow = concat $ fmap (take 1) xxs
        nextXXS = fmap (drop 1) xxs
    in [processRow curRow] ++ histogram' nextXXS

processRow :: [Integer] -> String
processRow xs = processRow' xs [0..9]
  where processRow' nums cols
          | length cols == 0 = []
          | (head cols) `elem` nums = '*' : processRow' nums (tail cols)
          | otherwise = ' ' : processRow' nums (tail cols)

module Fibonacci where

import qualified Data.Map as M

fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

fibs2 :: [Integer]
fibs2 = f 0 (M.fromList [])
  where f n m =
          let (fn, m') = fibs2' n m
          in fn : f (n+1) m'

fibs2' :: Integer -> M.Map Integer Integer -> (Integer, M.Map Integer Integer)
fibs2' 0 m = (0, m)
fibs2' 1 m = (1, m)
fibs2' n m = case M.lookup n m of
               Nothing ->
                 let fn = fst (fibs2' (n-1) m) + fst (fibs2' (n-2) m)
                 in (fn, M.insert n fn m)
               Just fn -> (fn, m)

data Stream a = Cons a (Stream a) deriving (Eq)

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Cons n (streamFromSeed f (f n))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = f 2 (streamRepeat 0)
  where f n s = f (n*2) (interleaveStreams s (nthStream n))

ruler' :: Stream Integer
ruler' =
  let zeros = streamRepeat 0
  in interleaveStreams zeros (nextStream 2)
  where nextStream n = interleaveStreams (nthStream n) (nextStream (n*2))

nthStream :: Integer -> Stream Integer
nthStream n = streamMap (\x -> if (x+1) `mod` n == 0
                               then 1
                               else 0) nats

interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
interleaveStreams (Cons a b) (Cons a' b') = Cons (a+a') (interleaveStreams b b')

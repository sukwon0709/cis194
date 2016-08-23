{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import           Data.Monoid
import           Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving Show

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l1 +++ l2 = Append ((tag l1) `mappend` (tag l2)) l1 l2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single m a)
  | n == 0 = Just a
  | otherwise = Nothing  
indexJ n (Append m l1 l2) =
  let l1Size = getSize . size $ tag l1
      l2Size = getSize . size $ tag l2
      totalSize = getSize . size $ m
  in if n >= totalSize
     then Nothing
     else if n < l1Size
          then indexJ n l1
          else indexJ (n - l1Size) l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n l@(Single _ _)
  | n <= 0 = l
  | otherwise = Empty
dropJ n l@(Append m l1 l2)
  | n <= 0 = l
  | otherwise =
    let l1Size = getSize . size $ tag l1
        l2Size = getSize . size $ tag l2
        totalSize = getSize . size $ m
    in if n > totalSize
       then Empty
       else if n <= l1Size
            then (dropJ n l1) +++ l2
            else (dropJ (n - l1Size) l2)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n l@(Single _ _)
  | n <= 0 = Empty
  | otherwise = l
takeJ n l@(Append m l1 l2)
  | n <= 0 = Empty
  | otherwise =
    let l1Size = getSize . size $ tag l1
        l2Size = getSize . size $ tag l2
        totalSize = getSize . size $ m
    in if n > totalSize
       then l
       else if n <= l1Size
            then takeJ n l1
            else l1 +++ (takeJ (n-l1Size) l2)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreFull :: String -> JoinList (Score, Size) String
scoreFull s = Single (scoreString s, 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l1 l2) = toString l1 ++ toString l2
  
  fromString s =
    let ls = lines s
        lists = fmap scoreFull ls
    in foldr (+++) Empty lists
  
  line n b = indexJ n b
  
  replaceLine n s b = takeJ (n-1) b +++ fromString s +++ dropJ n b
  
  numLines Empty = 0
  numLines (Single (_, l) _) = getSize l
  numLines (Append (_, l) _ _) = getSize l
  
  value Empty = 0
  value (Single (Score v, _) _) = fromIntegral v
  value (Append (Score v, _) _ _) = fromIntegral v

main = runEditor editor $ (buffer :: JoinList (Score, Size) String)
  where buffer = fromString $ unlines
          [ "This buffer is for notes you don't want to save, and for"
          , "evaluation of steam valve coefficients."
          , "To load a different file, type the character L followed"
          , "by the name of the file."
          ]

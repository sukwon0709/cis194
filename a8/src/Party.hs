module Party where

import           Control.Monad
import           Data.Monoid
import           Data.Tree
import           Employee


-- | Adds an Employee to the GuestList.
--   glCons should simply add the new Employee and their
--   fun scores without doing any checks.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL el f) = GL (e:el) (empFun e + f)

-- | Monoid instance for GuestList.
instance Monoid GuestList where
  mempty = GL [] 0
  (GL el1 f1) `mappend` (GL el2 f2) = GL (el1 ++ el2) (f1 + f2)

-- | Returns a more fun guest list.
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 >= gl2
                  then gl1
                  else gl2

-- | Fold for Data.Tree
treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f e t =
  let newAcc = f (rootLabel t) e
  in treeListFold (treeFold f) newAcc (subForest t)

treeListFold :: (b -> Tree a -> b) -> b -> [Tree a] -> b
treeListFold f acc ts = foldl f acc ts

treeFold' :: (a -> [b] -> b) -> Tree a -> b
treeFold' f t = f (rootLabel t) (fmap (treeFold' f) (subForest t))

nextLevel :: Employee
          -> [(GuestList, GuestList)]
          -> (GuestList, GuestList)
nextLevel e gls =
  let glsWithBoss = fmap fst gls
      glsWithoutBoss = fmap snd gls
      addedE = fmap (glCons e) glsWithoutBoss
  in (foldr moreFun mempty addedE,
      foldr moreFun mempty glsWithBoss)

nextLevel' :: Employee
           -> [(GuestList, GuestList)]
           -> (GuestList, GuestList)
nextLevel' boss [] = (GL [boss] (empFun boss), mempty)
nextLevel' boss gls =
  let withBoss = fmap fst gls
      withoutBoss = fmap snd gls
  in
    (
      foldr moreFun mempty (fmap (glCons boss) withoutBoss),
      foldr moreFun mempty withBoss
    )

maxFun :: Tree Employee -> GuestList
maxFun t =
  let topLevel = treeFold' nextLevel t
  in uncurry moreFun topLevel

printGuestList :: GuestList -> IO ()
printGuestList (GL el f) = do
  putStrLn ("Total fun: " ++ show f)
  void $ sequence $ fmap (putStrLn . empName) el

readGuestList :: FilePath -> IO GuestList
readGuestList fp = do
  contents <- readFile fp
  return $ maxFun (read contents)

main :: IO ()
main = do
  gl <- readGuestList "src/company.txt"
  printGuestList gl

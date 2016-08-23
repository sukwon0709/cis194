{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import           Control.Monad
import           Control.Monad.Random
import           Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show


battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  attacks <- replicateM (maxAttackingArmy bf) die
  defences <- replicateM (maxDefendingArmy bf) die
  let sortedAttacks = reverse $ sort attacks
      sortedDefences = reverse $ sort defences
      result = zipWith compare attacks defences
      numWins = foldr (\x xs -> if x == GT then 1 + xs else xs) 0 result
      numLoses = length result - numWins
  return $ Battlefield (attackers bf - numLoses) (defenders bf - numWins)


maxAttackingArmy :: Battlefield -> Army
maxAttackingArmy bf
  | attackers bf > 3 = 3
  | attackers bf > 0 = attackers bf - 1
  | otherwise = 0

maxDefendingArmy :: Battlefield -> Army
maxDefendingArmy bf
  | defenders bf > 1 = 2
  | otherwise = defenders bf

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  bf' <- battle bf
  if defenders bf' == 0 || attackers bf' < 2
    then return bf'
    else invade bf'

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 (invade bf)
  let numWins = foldr (\x xs -> if defenders x == 0 then 1 + xs else xs) 0 results
  return $ numWins / 1000

{-# LANGUAGE GADTs #-}

module Lib where

import qualified Data.Set.Monad as SM
import qualified Data.Set as S

-- Build an intermediate DataStructure to create the setMonad
data SetI a where
  Return :: a -> SetI a
  Bind   :: SetI a -> (a -> SetI b) -> SetI b
  Prim   :: (Ord a) => S.Set a -> SetI a

-- Interpreter to run the eval the datastructure in a Set
runSetI :: (Ord a) => SetI a -> S.Set a
runSetI (Prim s) = s
runSetI (Return a) = S.fromList [a]
runSetI (Bind (Prim s) f) = S.foldl' S.union S.empty (S.map (runSetI . f) s)
runSetI (Bind (Return a) f) = runSetI $ f a
runSetI (Bind (Bind ma f) g) = runSetI $ Bind ma (\x -> (Bind (f x) g))

instance (Show a, Ord a) => Show (SetI a) where
  show = show . runSetI

instance Functor SetI where
  fmap f s = Bind s (\x -> Return (f x))

instance Applicative SetI where
  pure = Return
  mf <*> mx = Bind mf (\f -> Bind mx (\x -> Return (f x)))

instance Monad SetI where
  return = Return
  (>>=) = Bind

s1 :: SM.Set Int
s1 = do
  a <- SM.fromList [1..10]
  b <- SM.fromList [0..20]
  return $ a + b

si1 :: SetI Int
si1 = do
  a <- Prim $ S.fromList [1..10]
  b <- Prim $ S.fromList [1..20]
  return (a + b)

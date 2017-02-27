{-# LANGUAGE GADTs #-}

module Lib where

import Control.Monad
import qualified Data.Set.Monad as SM
import qualified Data.Set as S
import           Data.Function            (on)

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

instance Ord a => Monoid (SetI a) where
  mempty = Prim S.empty
  mappend s1 s2 = Prim $ runSetI s1 `mappend` runSetI s2

liftSetI2 :: Ord a => (S.Set a -> S.Set a -> S.Set a) -> SetI a -> SetI a -> SetI a
liftSetI2 f s1 s2 = Prim $ f (runSetI s1) (runSetI s2)

union :: Ord a => SetI a -> SetI a -> SetI a
union = liftSetI2 S.union

empty :: Ord a => SetI a
empty = Prim $ S.empty

intersection :: Ord a => SetI a -> SetI a -> SetI a
intersection = liftSetI2 S.intersection

member :: Ord a => a -> SetI a -> Bool
member x s = S.member x $ runSetI s

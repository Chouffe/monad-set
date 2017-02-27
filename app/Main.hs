module Main where

import qualified Data.Set.Monad as SM
import qualified Data.Set as S

import Lib

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

main :: IO ()
main = do
  putStrLn "monad-set package"
  print s1
  putStrLn "my own monad-set implementation"
  print si1

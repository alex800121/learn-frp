{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module NatOrder where

import Control.Category
import Prelude hiding (id, (.))
import GHC.TypeLits (TypeError, ErrorMessage (..))
import Data.List (unfoldr)
import Control.Monad.Fix (fix)
import Control.Arrow

data Nat = Z | S Nat deriving (Show)

data (a :: Nat) :<=: (b :: Nat) where
  LTZ :: a :<=: a
  LTS :: a :<=: b -> a :<=: S b

deriving instance Show (a :<=: b)

instance Category (:<=:) where
  id = LTZ
  LTZ . b = b
  LTS a . b = LTS (a . b)

class ProjectLT a b where
  project :: a :<=: b

instance ProjectLT a a where
  project = LTZ

instance (ProjectLT a b) => ProjectLT a (S b) where
  project = LTS project


f :: S a :<=: S (S (S (S a)))
f = project

g :: S (S a) :<=: S (S a)
g = project

primes = loop (\(a, c) -> (c a, \(x : xs) -> x : c (filter ((/= 0) . (`mod` x)) xs))) [2 ..]

primes' = fix (\rec (x : xs) -> x : rec (filter ((/= 0) . (`mod` x)) xs)) [2 ..]

primes'' =
  unfoldr
    ( \case
        [] -> Nothing
        (x : xs) -> Just (x, filter ((/= 0) . (`mod` x)) xs)
    )
    [2 ..]

fib = loop (\(_, c) -> (c, 0 : 1 : zipWith (+) c (tail c))) ()

fib' = fix (\xs -> 0 : 1 : zipWith (+) xs (tail xs))

fib'' = 0 : scanl (+) 1 fib''

-- 0 : scanl (+) 1 (0 : _)
-- 0 : 1 : scanl (+) 1 (1 : _)
-- 0 : 1 : 1 : scanl (+) 2 (1 : _)
-- 0 : 1 : 1 : 2 : scanl (+) 3 (2 : _)
-- 0 : 1 : 1 : 2 : 3 : scanl (+) 5 (3 : _)
-- 0 : 1 : 1 : 2 : 3 : scanl (+) 5 (3 : _)
-- 0 : 1 : 1 : 2 : 3 : 5 : scanl (+) 8 (5 : _)

fib''' = unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)

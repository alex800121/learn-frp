module MyLib where

import Data.VectorSpace

type Time = Double

newtype Event a = Event {occ :: (Time, a)}

newtype Behavior a = Behavior {at :: Time -> a}

instance Functor Behavior where
  fmap f b = Behavior $ f . at b

instance Applicative Behavior where
  pure b = Behavior (const b)
  f <*> b = Behavior $ \t -> at f t (at b t)

time :: Behavior Time
time = Behavior id

timeTransformation :: Behavior a -> Behavior Time -> Behavior a
timeTransformation b t = Behavior $ at b . at t

-- newtype Behavior a = Behavior { at :: Time -> (a, Behavior a)}
-- instance Functor Behavior where
--   fmap f b =
--     Behavior $ \t ->
--       let (!a, b') = b `at` t
--           a' = f a
--        in (a', fmap f b')
-- instance Applicative Behavior where
--   pure x = Behavior $ const (x, pure x)
--   f <*> b =
--     Behavior $ \t ->
--       let (!a, b') = b `at` t
--           (fa, fb) = at f t
--           a' = fa a
--        in (a', fb <*> b')

main :: IO ()
main = print "Hello"

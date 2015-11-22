module State where

import Control.Monad

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) = ap

instance Monad (State s) where
  return = pure
  m >>= k = State $ \s -> let (a, s') = runState m s
                          in runState (k a) s'

stateFunction :: State [a] ()
stateFunction = do x <- pop
                   pop
                   push x

module State where

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (s, a)
runState (State m) s = m s

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State m) = State (\s -> let (x, s1) = m s in (f x, s1))

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure x = State (\s -> (x, s))

  -- (<*>) :: State s (a -> b) -> State s a -> State s b
  (State mf) <*> (State mx) = State (\s ->
    let (f, s1) = mf s
        (x, s2) = mx s1
    in (f x, s2))

instance Monad (State s) where
  -- return :: a -> State s a
  return = pure

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  (State m) >>= g = State (\s -> let (x, s1) = m s in runState (g x) s1)

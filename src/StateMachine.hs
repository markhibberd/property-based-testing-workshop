--
-- ==========================================
-- Example functions only:
--    Workshop Exercises are in 'test/Test'
-- ==========================================
--
module StateMachine (
    TurnstileState (..)
  , Turnstile (..)
  , newTurnstile
  , coin
  , push
  , insertCoin
  , pushTurnstile
  ) where

import           Data.IORef

data TurnstileState =
    Locked
  | Unlocked
    deriving (Eq, Ord, Show)

newtype Turnstile =
  Turnstile {
      unTurnstile :: IORef TurnstileState
    }

newTurnstile :: IO Turnstile
newTurnstile =
  Turnstile <$> newIORef Locked

coin :: TurnstileState -> TurnstileState
coin Locked = Unlocked
coin Unlocked = Unlocked

push :: TurnstileState -> TurnstileState
push Locked = Locked
push Unlocked = Locked


insertCoin :: Turnstile -> IO ()
insertCoin (Turnstile ref) =
  atomicModifyIORef' ref $ \_ ->
    (Unlocked, ())

pushTurnstile :: Turnstile -> IO Bool
pushTurnstile (Turnstile ref) =
  atomicModifyIORef' ref $ \s ->
    case s of
      Locked ->
        (Locked, False)
      Unlocked ->
        (Locked, True)

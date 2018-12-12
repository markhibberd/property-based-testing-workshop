{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
module Test.StateMachine where

import           StateMachine (Turnstile)
import qualified StateMachine

import           Control.Monad.IO.Class (MonadIO (..))

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

--
-- A State Machine Example by Tim Humphries:
--   https://teh.id.au/posts/2017/07/15/state-machine-testing/index.html
--

-- == Demonstration == --

--
-- This is currently not a part of the workshop, but is an example of
-- a very sophisticated use of property based testing that can be
-- discussed/used to demonstrate where things can be taken.
--


data ModelState (v :: * -> *) =
    TLocked
  | TUnlocked
    deriving (Eq, Ord, Show)

data Coin (v :: * -> *) =
  Coin
  deriving (Eq, Show)


instance HTraversable Coin where
  htraverse _ Coin =
    pure Coin


data Push (v :: * -> *) =
  Push
  deriving (Eq, Show)

instance HTraversable Push where
  htraverse _ Push =
    pure Push

initialState :: ModelState v
initialState =
    TLocked


s_coin :: (Monad n, MonadIO m, MonadTest m) => Turnstile -> Command n m ModelState
s_coin ts =
  let
    -- We can always insert a coin, so we don't need to see the state.
    gen _state =
      Just $
        pure Coin
    -- We execute this action by calling 'insertCoin'.
    execute Coin =
      liftIO (StateMachine.insertCoin ts)
  in
    Command gen execute [
        -- After a coin action, the turnstile should be unlocked.
        -- First we update our model:
        Update $ \_s Coin _o ->
          TUnlocked
        -- ... then we enforce a very simple predicate on it:
      , Ensure $ \_before after Coin () -> do
          after === TUnlocked
      ]


s_push_locked :: (Monad n, MonadIO m, MonadTest m) => Turnstile -> Command n m ModelState
s_push_locked ts =
  let
    -- This generator only succeeds when the gate is thought to be locked.
    gen state =
      case state of
        TLocked ->
          Just $
            pure Push
        TUnlocked ->
          Nothing
    execute Push = do
      liftIO $ StateMachine.pushTurnstile ts
  in
    Command gen execute [
        -- Precondition: the gate is thought to be locked.
        Require $ \s Push ->
          s == TLocked
        -- Update: pushing the locked gate has no effect.
      , Update $ \_s Push _o ->
          TLocked
        -- Postcondition: we're denied admission, the turnstile gate stays locked.
      , Ensure $ \before after Push b -> do
          before === TLocked
          assert (not b)
          after === TLocked
      ]


s_push_unlocked :: (Monad n, MonadIO m, MonadTest m) => Turnstile -> Command n m ModelState
s_push_unlocked ts =
  let
    gen state =
      case state of
        TUnlocked ->
          Just $
            pure Push
        TLocked ->
          Nothing
    execute Push = do
      liftIO $ StateMachine.pushTurnstile ts
  in
    Command gen execute [
        -- Precondition: the gate is thought to be unlocked.
        Require $ \s Push ->
          s == TUnlocked
        -- Update: pushing the unlocked gate locks it.
      , Update $ \_s Push _o ->
          TLocked
        -- Postcondition: we gain admission, the turnstile gate is locked.
      , Ensure $ \before after Push b -> do
          before === TUnlocked
          assert b
          after === TLocked
      ]


prop_turnstile :: Property
prop_turnstile =
  property $ do
    turnstile <- liftIO StateMachine.newTurnstile
    actions <- forAll $
      Gen.sequential (Range.linear 1 100) initialState [
          s_coin turnstile
        , s_push_locked turnstile
        , s_push_unlocked turnstile
        ]
    executeSequential initialState actions

tests :: IO Bool
tests =
  checkParallel $$(discover)

module Broker (
  Event, Broker, EventInfo,
  broker, emit, events, release, flush
) where

import Prelude

import Data.Array ((:))
import Effect (Effect)
import Effect.Exception (throw)
import Foreign.Generic.Class (class Decode, class Encode)
import InMemoryStore (Key, get, exists, (:=))
import InMemoryStore as M
import Broker.Either.Util (whenLeftM)
import Broker.Maybe.Util (whenNothing)

newtype Broker = Broker Key
-- | These are the records that are actually logged. Each one is just the name
-- | of some event and its payload.
type EventInfo a = {
  name :: String,
  -- time :: Milliseconds,
  dat :: a
}
-- | Wrap EventInfo to prevent misuse.
newtype Event a = Event (EventInfo a)

-- | Initialze the broker if it hasn't been init'd. The type of the array
-- | here is a lie, but it doesn't actually matter and spelling it correctly
-- | would probably require the use of proxies and shit for no particular
-- | reason.
broker :: Key -> Effect Broker
broker k = do
  unlessM (exists k) (k := ([] :: Array Unit))

  pure (Broker k)

-- | Clean the memory store by deleting any existing values.
flush :: Effect Unit
flush = M.flush

-- | Clean up the broker key so that it can be reallocated. This is deprecated
-- | IIRC.
release :: Broker -> Effect Unit
release (Broker k) = M.release k

-- | Log an event.
emit :: ∀ a. Encode a => Decode a => Broker -> String -> a -> Effect Unit
emit b@(Broker k) name dat = do
  let e = {
    name,
    -- time: Milliseconds 1.0,
    dat
  }
  store <- getS b
  k := (e : store)

getS :: ∀ a. Decode a => Encode a => Broker -> Effect (Array (EventInfo a))
getS (Broker k) = whenLeftM (get k) decodeErr >>= (flip whenNothing) initErr
  where
  initErr =
    throw $
      "Tried to get a value, but the broker hadn't been initialized."
      <> " This shouldn't happen."
  decodeErr errs =
    throw $
      "Failed to decode EventInfo from the memory store: "
      <> show errs

-- | List the events that have been loged so far.
events :: ∀ a. Encode a => Decode a => Broker -> Effect (Array (EventInfo a))
events b = getS b

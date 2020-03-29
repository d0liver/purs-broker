module Broker (
  Event, Broker, EventInfo,
  broker, emit, events, release, flush
) where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Foreign.Generic.Class (class Decode, class Encode)
import InMemoryStore (Key, get, exists, (:=))
import InMemoryStore as M
import Partial.Unsafe (unsafeCrashWith)

newtype Broker = Broker Key
type EventInfo a = {
  name :: String,
  -- time :: Milliseconds,
  dat :: a
}
newtype Event a = Event (EventInfo a)

broker :: Key -> Effect Broker
broker k = do
  b <- exists k
  when b $
    -- This shouldn't normally happen so I'm okay throwing here.
    throw $
      "Tried to allocate a broker to a key, '"
      <> k <> "', that was already defined."

  let brk = Broker k
  -- Initialize the broker here to prevent double initializations later. The
  -- type of the array here is a lie, but it doesn't actually matter and
  -- spelling it correctly would probably require the use of proxies and shit
  -- for no particular reason.
  k := ([] :: Array Unit)
  pure brk

flush :: Effect Unit
flush = M.flush

release :: Broker -> Effect Unit
release (Broker k) = M.release k

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
getS (Broker k) = do
  f <- get k
  maybeStore <- case f of
    Left errs -> throw ("Failed to decode EventInfo from the memory store: " <> show errs)
    Right r -> pure r
  case maybeStore of
    Nothing ->
      unsafeCrashWith $
        "Tried to get a value, but the broker hadn't been initialized."
        <> " This shouldn't happen."
    Just store -> pure store

events :: ∀ a. Encode a => Decode a => Broker -> Effect (Array (EventInfo a))
events b = getS b

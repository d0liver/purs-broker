module Maybe.Util (
  whenJust, whenNothing,
  whenJustM, whenNothingM
) where

import Prelude
import Data.Maybe (Maybe, maybe)

whenNothing :: forall m a. Applicative m => Maybe a -> m a -> m a
whenNothing m eff = maybe eff pure m

whenJust :: ∀ m a. Applicative m => Maybe a -> (a -> m Unit) -> m Unit
whenJust mb f = maybe (pure unit) f mb

whenJustM :: forall m a. Bind m => Applicative m => m (Maybe a) -> (a -> m Unit) -> m Unit
whenJustM v f = v >>= (flip whenJust) f

whenNothingM :: forall m a. Bind m => Applicative m => m (Maybe a) -> m a -> m a
whenNothingM v f = v >>= (flip whenNothing) f

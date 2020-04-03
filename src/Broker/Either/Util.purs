module Either.Util where

import Prelude
import Data.Either (Either(..), either)

prefixLeft :: ∀ a. String -> Either String a -> Either String a
prefixLeft pref (Left s) = Left (pref <> s)
prefixLeft pref r = r

whenLeft :: ∀ a b m. Applicative m => Either b a -> (b -> m a) -> m a
whenLeft m f = either f pure m

whenLeftM :: forall b m a. Bind m => Applicative m => m (Either b a) -> (b -> m a) -> m a
whenLeftM m f = m >>= (flip whenLeft) f

module Hayoo.Internal.Helpers
  ( (|>)
  ) where



-- HELPERS


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a

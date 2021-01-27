module Lightarrow.GridField.FieldSign where

import FRP.BearRiver

class FieldSign a where
    type Prose a
    cueProse :: Prose a -> a
    reactProse :: (Prose a -> b) -> a -> Event b

instance FieldSign a => FieldSign (Event a) where
    type Prose (Event a) = Prose a
    cueProse                = Event . cueProse
    reactProse f (Event a)  = reactProse f a
    reactProse _f NoEvent   = NoEvent

instance FieldSign a => FieldSign [a] where
    type Prose [a] = Prose a
    cueProse p              = [cueProse p]
    reactProse f as         = mergeEvents (map (reactProse f) as)
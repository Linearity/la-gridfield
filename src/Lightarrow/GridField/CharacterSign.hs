module Lightarrow.GridField.CharacterSign where

import FRP.BearRiver

import Lightarrow.GridField.CardinalDirection

-- | Types that represent commands to characters
class CharacterSign a where
    characterMove           :: CardinalDirection -> a
    reactCharacterMove      :: (CardinalDirection -> b) -> a -> Event b
    characterStop           :: a
    reactCharacterStop      :: b -> a -> Event b
    characterSpeak          :: Int -> CardinalDirection -> a
    reactCharacterSpeak     :: (Int -> CardinalDirection -> b) -> a -> Event b

instance CharacterSign a => CharacterSign (Event a) where
    characterMove d                     = Event (characterMove d)
    reactCharacterMove f (Event a)      = reactCharacterMove f a
    reactCharacterMove _f _             = NoEvent
    characterStop                       = Event characterStop
    reactCharacterStop b (Event a)      = reactCharacterStop b a
    reactCharacterStop _b _             = NoEvent 
    characterSpeak                      = (Event .) . characterSpeak
    reactCharacterSpeak f (Event a)     = reactCharacterSpeak f a
    reactCharacterSpeak _f _            = NoEvent 

instance CharacterSign a => CharacterSign [a] where
    characterMove d             = [characterMove d]
    reactCharacterMove f as     = mergeEvents (map (reactCharacterMove f) as)
    characterStop               = [characterStop]
    reactCharacterStop b as     = mergeEvents (map (reactCharacterStop b) as)
    characterSpeak              = ((:[]) .) . characterSpeak
    reactCharacterSpeak f as    = mergeEvents (map (reactCharacterSpeak f) as)
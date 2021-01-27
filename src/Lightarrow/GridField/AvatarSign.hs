module Lightarrow.GridField.AvatarSign where

import FRP.BearRiver

import Lightarrow.GridField.CardinalDirection
import Lightarrow.GridField.CharacterSign

-- | Types that can represent cues about the avatar
class CharacterSign a => AvatarSign a where
    avatarInspect           :: CardinalDirection -> a
    reactAvatarInspect      :: (CardinalDirection -> b) -> a -> Event b
    avatarTraverse          :: a
    reactAvatarTraverse     :: b -> a -> Event b

instance AvatarSign a => AvatarSign (Event a) where
    avatarInspect                       = Event . avatarInspect
    reactAvatarInspect b (Event a)      = reactAvatarInspect b a
    reactAvatarInspect _b NoEvent       = NoEvent
    avatarTraverse                      = Event avatarTraverse
    reactAvatarTraverse b (Event a)     = reactAvatarTraverse b a
    reactAvatarTraverse _b NoEvent      = NoEvent

instance AvatarSign a => AvatarSign [a] where
    avatarInspect               = (:[]) . avatarInspect
    reactAvatarInspect _b []    = NoEvent
    reactAvatarInspect b as     = mergeEvents (map (reactAvatarInspect b) as)
    avatarTraverse              = [avatarTraverse]
    reactAvatarTraverse _b []   = NoEvent
    reactAvatarTraverse b as    = mergeEvents (map (reactAvatarTraverse b) as)
module Lightarrow.GridField.Avatar where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Function
import Data.Maybe
import FRP.BearRiver hiding (first)
import Lightarrow
import Linear
import Linear.Affine
import Optics

import Lightarrow.GridField.Action
import Lightarrow.GridField.AvatarSign
import Lightarrow.GridField.CardinalDirection
import Lightarrow.GridField.Character
import Lightarrow.GridField.CharacterSign
import Lightarrow.GridField.FieldSign
import Lightarrow.GridField.TerrainElement
import Lightarrow.GridField.Terrain

class AvatarState a c p where
    avatarState :: Lens' a (Character
                            (FieldT a c p)
                            (SceneGraph Double (Actuation p)))

avatar terr0
        = do    a0 <- gets (view avatarState)
                loop (faceForward a0)
    where   loop a  = do    (_, a') <- avatarStop terr0 a
                            loop a'

type AvatarTypes cue location platform prose state
    = ( AvatarSign cue,
        AvatarState state [cue] platform,
        CharacterSign cue,
        Done cue,
        FieldSign cue,
        MonadFix platform,
        Teleport location cue,
        TerrainCode platform,
        RectanglePlatform platform          )

avatarStop :: AvatarTypes cue location platform prose state =>
                Terrain platform [cue]
                    -> FieldCharacter state [cue] platform
                    -> Task
                        [cue]
                        (   FieldCharacter state [cue] platform,
                            Event [cue]                         )
                        (FieldT state [cue] platform)
                        (   [cue],
                            FieldCharacter state [cue] platform  )
avatarStop terr0 c0
    = do    (cue1, c1)  <- standing c0
                            & silent
                            . onlyUntil (arr (\(cue, c) ->
                                                attach (filterInspect cue) c)
                                            >>> notYet)
            (modifier, _)   <- lift ask
            if  | Event d   <- reactCharacterMove id cue1
                    -> avatarMove terr0 (march d c1)
                | Event ()      <- reactAvatarInspect (const ()) cue1,
                  Event cue2    <- inspectReaction c1 (modifier `appEndo` terr0)
                    -> if   | Event (k, d) <- reactCharacterSpeak (,) cue2
                                -> do   tell (Endo id, characterSpeak k d)
                                        avatarStop terr0 c1
                            | Event _ <- reactProse id cue2
                                -> do   modify (set avatarState c1)
                                        momentC (c1, Event cue2)
                                        return (cueDone, c1)
                            | otherwise
                                -> return (cueDone, c1)
                | otherwise
                    -> return (cue1, c1)
    where   filterInspect
                = reactAvatarInspect avatarInspect
            inspectReaction char terr
                = do    te  <- maybeToEvent (terrainInFront char terr)
                        reactWith (teReactions te)
                            (avatarInspect (cDirection char))

avatarMove :: AvatarTypes cue location platform prose state =>
                Terrain platform [cue]
                    -> FieldCharacter state [cue] platform
                    -> Task
                        [cue]
                        (   FieldCharacter state [cue] platform,
                            Event [cue]                         )
                        (FieldT state [cue] platform)
                        (   [cue],
                            FieldCharacter state [cue] platform  )
avatarMove terr0 c0
    = do    (cue, c1)   <- walking terr0 c0
                            & silent
            (modifier, _)   <- lift ask
            let reactions = [   reactCharacterStop
                                    (return (cue, faceForward c1)),
                                reactCharacterMove
                                    (\d -> if d /= cDirection c1
                                            then avatarMove terr0 (march d c1)
                                            else avatarMove terr0 c1)             ]
            if  | Event cue2    <- traversalReaction c1 (modifier `appEndo` terr0),
                  Event m       <- reactTeleport id cue2
                    -> do   modify (set avatarState c1)
                            momentC (c1, cueTeleport m)
                            return (cueDone, c1)
                | otherwise
                    -> reactAll (reactWith reactions) cue
                        & fromEvent (avatarMove terr0 c1)
    where traversalReaction char terr
            = do    te  <- maybeToEvent (terrainUnder char terr)
                    reactWith (teReactions te) avatarTraverse

avatarControl :: (Monad m, AvatarSign c, CharacterSign c, KeyboardPlatform p) =>
                    SF m (Sensation p) [c] 
avatarControl = proc s -> do
                    let z           = keyPressed (charKey 'z') s
                        down        = keyPressed arrowDownKey s
                        left        = keyPressed arrowLeftKey s
                        right       = keyPressed arrowRightKey s
                        up          = keyPressed arrowUpKey s
                        noArrow     = not (or [up, left, down, right])
                    zPress      <- edge         -< z
                    upPress     <- iEdge False  -< up
                    leftPress   <- iEdge False  -< left
                    downPress   <- iEdge False  -< down
                    rightPress  <- iEdge False  -< right
                    release     <- edge         -< noArrow
                    returnA -< mapMaybe eventToMaybe
                                [   tag zPress (avatarInspect South),   -- direction is irrelevant
                                    tag upPress (characterMove North),
                                    tag leftPress (characterMove West),
                                    tag downPress (characterMove South),
                                    tag rightPress (characterMove East),
                                    tag release characterStop   ]
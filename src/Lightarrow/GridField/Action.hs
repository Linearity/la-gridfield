module Lightarrow.GridField.Action
    (   FieldAction,
        FieldCharacter,
        FieldT,
        currentTerrain,
        avatarDraw,
        npcDraw,
        silent,
        noInput,
        inputFromBus,
        addInputFromBus     ) where

import qualified Control.Arrow as A (first)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Monoid
import FRP.BearRiver hiding (first)
import Lightarrow
import Linear

import Lightarrow.GridField.Actuation
import Lightarrow.GridField.Character
import Lightarrow.GridField.Terrain
import Lightarrow.GridField.TerrainElement

type FieldAction inp state c p ret
    = Task
        inp
        (SceneGraph Double (Actuation p), Event c)
        (FieldT state c p)
        ret

type FieldCharacter s c p
    = Character (FieldT s c p) (SceneGraph Double (Actuation p))

type FieldT state c p
    = StateT state (BusT (Endo (Terrain p c), c) p)

currentTerrain :: (Monad p, Monoid c, BlitPlatform p, TerrainTile p) =>
                    Terrain p c -> FieldAction state inp c p b
currentTerrain terr0 = always (constM askBus >>> arr (draw . fst))
    where draw modifier = ( Node (Frame (translate (V3 0 0 (-1))))
                                [drawTerrain (modifier `appEndo` terr0)],
                            NoEvent                                         )

avatarDraw :: (Monad m, Monoid c, RectanglePlatform p) =>
                Terrain p c
                    -> Task
                        inp
                        (   Character m (SceneGraph Double (Actuation p)),
                            Event c                                         )
                        (FieldT state c p)
                        ret
                    -> FieldAction inp state c p ret
avatarDraw terr0 = mapTask (>>> arr (first (first draw)))
    where draw = addCamera (tBackgroundColor terr0) . drawCharacter

npcDraw :: (Monad m, Monad p, Monoid c) =>
            Task    inp
                    (   Character m (SceneGraph Double (Actuation p)),
                        Event c                                         )
                    (FieldT state c p)
                    ret
                -> FieldAction inp state c p ret
npcDraw = mapTask (>>> arr (first (first drawCharacter)))

silent :: Monad m => Task d b m f -> Task d (b, Event a) m f
silent = mapTask (>>> arr (first (, NoEvent)))

noInput :: (Monad p, Monoid c) =>
            Task c b (FieldT state c p) d
                -> Task a b (FieldT state c p) d
noInput = mapTask (constant mempty >>>)

inputFromBus :: MonadReader (e, a) m => Task a b m c -> Task d b m c
inputFromBus = mapTask (constM (lift (asks snd)) >>>)

addInputFromBus :: (Semigroup a, MonadReader (e, a) m) =>
                        Task a b m c -> Task a b m c
addInputFromBus = mapTask (\sf ->
                                proc cue1 -> do
                                    cue2 <- constM (lift (asks snd)) -< ()
                                    sf -< cue1 <> cue2)
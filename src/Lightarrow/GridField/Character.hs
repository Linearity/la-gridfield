module Lightarrow.GridField.Character
        (   Character (..),
            Animation,
            CharacterEnv,
            CharacterAction,
            oppositeDirection,
            displacement,
            CharacterGaits (..),
            CharacterStances (..),
            faceForward,
            face,
            march,
            marchForward,
            gait,
            stance,
            standing,
            walking,
            stampeding,
            blocked,
            terrainInFront,
            terrainUnder            ) where

import qualified Control.Arrow as A
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Function
import Data.Tuple
import FRP.BearRiver
import Lightarrow
import Linear
import Linear.Affine

import Lightarrow.GridField.AvatarSign
import Lightarrow.GridField.CardinalDirection
import Lightarrow.GridField.CharacterSign
import Lightarrow.GridField.Terrain
import Lightarrow.GridField.TerrainElement

-- | Animated figure on the field that moves around
data Character m b = Character {
                            cAnimation  :: !(Animation m b),
                            cDirection  :: !CardinalDirection,
                            cGaits      :: !(CharacterGaits m b),
                            cIndex      :: !Int,
                            cLocation   :: !(Point V2 Double),
                            cOutput     :: !b,
                            cSpeed      :: !Double,
                            cStances    :: !(CharacterStances m b)
                        }

instance Show (Character m b) where
    show c = "Character {" ++
                " cDirection = " ++ show (cDirection c) ++
                " cLocation = " ++ show (cLocation c) ++
                " cSpeed = " ++ show (cSpeed c) ++
                " }"

-- | Character sprite animation
type Animation m b = SF m () b 


-- | Walking animations for a character
data CharacterGaits m b = CharacterGaits {
                                cgEastGait      :: !(Animation m b),
                                cgNorthGait     :: !(Animation m b),
                                cgSouthGait     :: !(Animation m b),
                                cgWestGait      :: !(Animation m b)
                            }

-- | Standing animations for a character
data CharacterStances m b = CharacterStances {
                                    cgEastStance    :: !(Animation m b),
                                    cgNorthStance   :: !(Animation m b),
                                    cgSouthStance   :: !(Animation m b),
                                    cgWestStance    :: !(Animation m b)
                                }

type BusEnv a m = (MonadReader a m, MonadWriter a m, Monoid a)

-- type CharacterEnv cInRet cBus p m
type CharacterEnv c p m
    = ( Monoid c,
        BusEnv (Endo (Terrain p c), c) m,
        TerrainCode p,
        MonadFix m,
        CharacterSign c                 )

type CharacterAction c p m
    = Task
        c
        (Character m (SceneGraph Double (Actuation p)))
        m
        (c, Character m (SceneGraph Double (Actuation p)))

-- | Start a character's standing animation
faceForward :: Character m b -> Character m b
faceForward c = face (cDirection c) c

-- | Set a character's direction and corresponding standing animation
face :: CardinalDirection -> Character m b -> Character m b
face newDirection c = c' { cAnimation = stance c' }
    where c' = c { cDirection = newDirection }

-- | Start a character's walking animation
marchForward :: Character m b -> Character m b
marchForward c = march (cDirection c) c

-- | Set a character's direction and corresponding walking animation
march :: CardinalDirection -> Character m b -> Character m b
march newDirection c = c' { cAnimation = gait c' }
    where c' = c { cDirection = newDirection }

-- | A character's walking animation corresponding to its direction
gait :: Character m b -> Animation m b
gait c = case cDirection c of
            East    -> cgEastGait gs
            North   -> cgNorthGait gs
            South   -> cgSouthGait gs
            West    -> cgWestGait gs
    where gs = cGaits c

-- | A character's standing animation corresponding to its direction
stance :: Character m b -> Animation m b
stance c = case cDirection c of
            East    -> cgEastStance ss
            North   -> cgNorthStance ss
            South   -> cgSouthStance ss
            West    -> cgWestStance ss
    where ss = cStances c

-- | A character standing until it receives a cue to speak, move, or stop
standing :: (CharacterEnv c p m, AvatarSign c) =>
                Character m (SceneGraph Double (Actuation p))
                    -> CharacterAction c p m
standing c0 = stand c0
                & lastOut c0
                . onlyUntil (arr (reactWith filters . fst) >>> notYet)
    where   filters = [     reactCharacterSpeak characterSpeak,
                            reactCharacterMove characterMove,
                            reactCharacterStop characterStop    ]

-- | Animate a character standing, in a constant location, facing in the most
-- recent direction in which it walked.
stand :: (Monoid b1, MonadWriter (Endo (Terrain p c), b1) m, TerrainCode p, AvatarSign c)
            => Character m b2
                -> Task a (Character m b2) m c2
stand c = always (constM (occupyBus c)
                    >>> constM (awaitSpeechBus c)
                    >>> constant (cLocation c)
                    >>> animate c)

-- | A character walking for one tile, and then reacting to the last command
-- it received while walking.  This may amount to 'walking' in the same or
-- different direction if it received a movement command, or 'standing' in
-- place if it received a stop command.
walking :: CharacterEnv c p m =>
                Terrain p c
                    -> Character m (SceneGraph Double (Actuation p))
                    -> CharacterAction c p m
walking terr0 c0
    = do    (modifier, _) <- lift ask
            if blocked c0 (modifier `appEndo` terr0)
                then return (characterStop, c0)
                else walk c0
                        & mapTask (>>> right (arr swap))
                        . mapTask accumulateInput

stampeding :: CharacterEnv c p m =>
                Character m (SceneGraph Double (Actuation p))
                    -> CharacterAction c p m
stampeding c0 = do  walk c0
                        & mapTask (>>> right (arr swap))
                        . mapTask accumulateInput

-- | Animate a character walking in its current direction over the length of
-- one tile, and return the character at its destination.
walk :: (MonadFix m, Monoid b1, MonadWriter (Endo (Terrain p c), b1) m, TerrainCode p)
            => Character m b2
                -> Task a (Character m b2) m (Character m b2)
walk c0 = do    ((), c1)    <- slide x0 x1 (1 / cSpeed c0)
                                    & lastOut c0
                                    . mapTask (>>> left (arrM (\c ->
                                                                do  occupyBus c
                                                                    return c)))
                                    . mapTask (>>> left (animate c0))
                return c1 { cLocation = x1 }
    where   x0          = cLocation c0
            x1          = x0 .+^ displacement (cDirection c0)

-- | Modify a task's representative to accumulate a monoidal input, returning
-- the latest accumulated value
accumulateInput :: (Monoid a, MonadFix m) =>
                    MSF m a (Either d b) -> MSF m a (Either d (b, a))
accumulateInput sf = proc a -> do
                        rec a'      <- iPre mempty  -< a <> a'
                        eOutDone    <- sf           -< a
                        returnA -< right (, a') eOutDone

-- | A signal function that runs a given character's animation.  Its output
-- signal carries the character whose location is equal to the input value,
-- whose output value is equal to the latest output of the animation, and
-- whose animation is equal to the latest continuation of the animation.
animate :: Monad m => Character m b -> SF m (Point V2 Double) (Character m b)
animate c = proc x -> do
                (out, k) <- vain (cAnimation c) -< ()
                returnA -< c {  cAnimation  = k,
                                cOutput     = out,
                                cLocation   = x     }

occupyBus :: (Monoid b1, MonadWriter (Endo (Terrain p c), b1) m1, TerrainCode p)
                => Character m2 b2 -> ClockInfo m1 ()
occupyBus c = tellBus (Endo (occupy (round x) (round y)), mempty)
    where P (V2 x y) = cLocation c

awaitSpeechBus :: (Monoid b1, MonadWriter (Endo (Terrain p c), b1) m1, TerrainCode p, AvatarSign c)
                    => Character m2 b2 -> ClockInfo m1 ()
awaitSpeechBus c = tellBus (Endo update, mempty)
    where   update terr = setElem
                            (maybe
                                (defaultTerrainElement { teReactions = [speak] })
                                (\te -> te { teReactions = speak : teReactions te })
                                (getElem (round x) (round y) terr))
                            (round x)
                            (round y)
                            terr
            speak       = reactAvatarInspect
                            (characterSpeak (cIndex c) . oppositeDirection)
            P (V2 x y)  = cLocation c

blocked :: Character m b -> Terrain p c -> Bool
blocked c = maybe False teCollides . terrainInFront c

terrainInFront :: Character m b -> Terrain p c -> Maybe (TerrainElement p c)
terrainInFront c = getElem (round (x + dx)) (round (y + dy))
    where   P (V2 x y)     = cLocation c
            V2 dx dy       = displacement (cDirection c)

terrainUnder :: Character m b -> Terrain p c -> Maybe (TerrainElement p c)
terrainUnder c = getElem (round x) (round y)
    where P (V2 x y) = cLocation c

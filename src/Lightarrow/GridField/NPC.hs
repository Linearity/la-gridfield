{-# LANGUAGE AllowAmbiguousTypes #-}

module Lightarrow.GridField.NPC where

import qualified Control.Arrow as A
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Function
import FRP.BearRiver hiding (first)
import Lightarrow
import Linear hiding (trace)
import Linear.Affine
import System.Random.TF.Instances
import System.Random.TF.Gen

import Lightarrow.GridField.Action
import Lightarrow.GridField.AvatarSign
import Lightarrow.GridField.CardinalDirection
import Lightarrow.GridField.Character
import Lightarrow.GridField.CharacterSign
import Lightarrow.GridField.FieldSign
import Lightarrow.GridField.Terrain
import Lightarrow.GridField.TerrainElement

type NPCTypes cue platform state
    = ( AvatarSign cue,
        CharacterSign cue,
        Done cue,
        FieldSign cue,
        MonadFix platform,
        TerrainCode platform,
        RandomGen state,
        RectanglePlatform platform  )

greeter :: NPCTypes cue platform state =>
                Terrain platform [cue]
                    -> Prose cue
                    -> FieldCharacter state [cue] platform
                    -> FieldAction
                        input
                        state
                        [cue]
                        platform
                        ([cue], FieldCharacter state [cue] platform)
greeter terr0 p c = script (faceForward c)
                        & npcDraw
                        . inputFromBus
    where   script c0 = do
                (cue, c1) <- standing c0
                                & mapTask (>>> arr (first (, NoEvent)))
                if  | Event (k, d) <- reactCharacterSpeak (,) cue,
                      k == cIndex c0
                        -> do   c2 <- turnAndSpeak terr0 p c1 d
                                let c3 = face (cDirection c0) c2
                                tWaitMs <- state (randomR (1000, 3000))
                                let dt = fromIntegral (tWaitMs :: Int) / 1000
                                script c2
                                    & onlyUntil (after dt (cueDone, c3))
                    | otherwise
                        -> return (cue, c1)

wanderer :: NPCTypes cue platform state =>
                Terrain platform [cue]
                    -> (Int, Int)
                    -> (Point V2 Double, V2 Double)
                    -> Prose cue
                    -> FieldCharacter state [cue] platform
                    -> FieldAction
                        input
                        state
                        [cue]
                        platform
                        ([cue], FieldCharacter state [cue] platform)
wanderer terr0 timeBounds spaceBounds p c0 = script (faceForward c0)
                                                & npcDraw
                                                . inputFromBus
    where script c0
            = do    (cue, c1)   <- wandering terr0 timeBounds spaceBounds c0
                                    & silent
                    if  | Event (k, d) <- reactCharacterSpeak (,) cue,
                          k == cIndex c0
                            -> do   c2 <- turnAndSpeak terr0 p c1 d
                                    return (cueDone, c2)
                        | otherwise
                            -> return (cue, c1)

approacher :: NPCTypes cue platform state =>
                Terrain platform [cue]
                    -> Point V2 Double
                    -> FieldCharacter state [cue] platform
                    -> FieldAction
                        input
                        state
                        [cue]
                        platform
                        ([cue], FieldCharacter state [cue] platform)
approacher terr0 pT c0 = approaching terr0 pT c0
                            & npcDraw
                            . silent
                            . noInput

wandering :: (  AvatarSign cue,
                CharacterEnv cue platform m,
                Done cue,
                MonadState state m,
                RandomGen state                 ) =>
                    Terrain platform cue
                        -> (Int, Int)
                        -> (Point V2 Double, V2 Double)
                        -> Character m (SceneGraph Double (Actuation platform))
                        -> CharacterAction cue platform m
wandering terr0 timeBounds spaceBounds c0
    = do    tWaitMs     <- state (randomR timeBounds)
            let tWait = fromIntegral (tWaitMs :: Int) / 1000
            (cue, c1)   <- standing (faceForward c0)
                            & onlyUntil (proc (a, b) -> do
                                            done <- after tWait cueDone -< ()
                                            returnA -< done `attach` b)
            if  | Event () <- reactDone () cue
                    -> do   nSteps  <- state (randomR (1, 3))
                            indexD  <- state (randomR (0, 3))
                            (f, _)  <- lift ask
                            let d   = toEnum indexD
                            loop (nSteps :: Int) (march d c1)
                | otherwise
                    -> return (cue, c1)
    where   loop steps c0
                | facingOut c0  = return (characterStop, c0)
                | steps == 1    = walking terr0 c0
                | otherwise     = do    (cue, c1)  <- walking terr0 c0
                                        if isEvent (reactCharacterStop () cue)
                                            then return (cue, c1)
                                            else loop (steps - 1) c1
            facingOut c         = not . pointInBox2 spaceBounds 0
                                    $ cLocation c .+^ displacement (cDirection c)

approaching :: (    AvatarSign cue,
                    CharacterEnv cue platform m,
                    Done cue,
                    MonadState state m,
                    RandomGen state                 ) =>
                        Terrain platform cue
                            -> Point V2 Double
                            -> Character m (SceneGraph Double (Actuation platform))
                            -> CharacterAction cue platform m
approaching terr0 pT@(P (V2 xT yT)) c0
    | pC == pT      = return (characterStop, c0)
    | otherwise     = do    let dx                      = xT - xC
                                dy                      = yT - yC
                                dir | abs dy > abs dx   = if dy > 0
                                                              then South
                                                              else North
                                    | dx > 0            = East
                                    | otherwise         = West
                                c1  | dir /= d0         = march dir c0
                                    | otherwise         = c0
                            (cue, c2) <- walking terr0 c1
                            doReactWith
                                [ reactCharacterStop
                                    (return (cue, faceForward c2))  ]
                                (approaching terr0 pT c2)
                                cue
    where   pC@(P (V2 xC yC))   = cLocation c0
            d0                  = cDirection c0

turnAndSpeak terr0 p c0 d
        = do    (_, c1) <- standing (face d c0)
                                & silent
                                . mapTask sendStopImmediately
                momentC (c1, cueProse p)
                return c1
    where sendStopImmediately = (constant characterStop >>>)

remoteControl :: (  AvatarSign cue,
                    CharacterEnv [cue] platform m,
                    Done [cue],
                    MonadState state m,
                    RandomGen state                 ) =>
                        Terrain platform [cue]
                            -> Character m (SceneGraph Double (Actuation platform))
                            -> CharacterAction [cue] platform m
remoteControl terr0 c0 = npcStop (faceForward c0)
    where   npcStop c0 = do (cue, c1) <- standing c0
                            if  | Event d <- reactCharacterMove id cue
                                    -> npcMove (march d c1)
                                | otherwise
                                    -> return (cue, c1)
            npcMove c0 = do (cue, c1)   <- walking terr0 c0
                            let reactions = [   reactCharacterStop
                                                    (return (cue, faceForward c1)),
                                                reactCharacterMove
                                                    (\d -> if d /= cDirection c1
                                                            then npcMove (march d c1)
                                                            else npcMove c1)             ]
                            reactAll (reactWith reactions) cue
                                & fromEvent (npcMove c1)

module Lightarrow.GridField.Terrain
        (   Terrain (..),
            defaultTerrain,
            mapWithIndices,
            empty,
            getElem,
            setElem,
            adjustElem,
            deleteElem,
            occupy          ) where

import qualified Data.Map as M
import Data.Maybe
import Lightarrow

import Lightarrow.GridField.TerrainElement

data Terrain p c = Terrain {
                        tBackgroundColor :: Color,
                        tElements :: M.Map (Int, Int) (TerrainElement p c)
                    }

defaultTerrain = Terrain {
                        tBackgroundColor  = Magenta,
                        tElements         = M.empty
                    }

mapWithIndices :: (k -> a -> b) -> M.Map k a -> M.Map k b
mapWithIndices = M.mapWithKey

empty :: Terrain p c
empty = Terrain White M.empty

getElem :: Int -> Int -> Terrain p c -> Maybe (TerrainElement p c)
getElem x y = (M.!? (y, x)) . tElements

setElem :: TerrainElement p c -> Int -> Int -> Terrain p c -> Terrain p c
setElem elem x y t@(Terrain _ es) = t { tElements = M.insert (y, x) elem es }

adjustElem :: (TerrainElement p c -> TerrainElement p c) -> Int -> Int -> Terrain p c -> Terrain p c
adjustElem f x y t@(Terrain _ es) = t { tElements = M.adjust f (y, x) es }

deleteElem :: Int -> Int -> Terrain p c -> Terrain p c
deleteElem x y t@(Terrain _ es) = t { tElements = M.update (const Nothing) (y, x) es }

occupy :: TerrainCode p => Int -> Int -> Terrain p c -> Terrain p c
occupy x y t = setElem (solidify (fromMaybe defaultTerrainElement (getElem x y t))) x y t
  where
    solidify te = te { teCollides = True }

module Lightarrow.GridField.Actuation where

import qualified Data.Map as M
import Data.Foldable
import Data.Tree
import Lightarrow
import Linear
import Optics

import Lightarrow.GridField.Terrain
import Lightarrow.GridField.TerrainElement
import Lightarrow.GridField.Character

drawCharacter Character { cOutput = draw, cLocation = p }
    = Node (Frame (locate2 p & _translation %~ ((^-^ V3 0 4 0) . (16 *^)))) -- Num instance for vectors
        [draw]

drawTerrainElement TerrainElement { teTile = Nothing }
    = mempty
drawTerrainElement TerrainElement { teTile = Just (tile, orientation) }
    = drawTile tile orientation

drawTerrain terrain = fold (mapWithIndices tiler (tElements terrain))
  where
    tiler (r, c) element    = Node (Frame (place r c))
                                [drawTerrainElement element]
    place r c               = translate2 (V2    (fromIntegral $ 16 * c)
                                                (fromIntegral $ 16 * r))

addCamera bgColor = _kids %~ (camera :)
    where camera = Node (Frame (translate2 (V2 (-64) (-64))))
                    [   Node Camera [],
                        bgFill bgColor  ]

-- | Draw a rectangle behind everything.
bgFill :: RectanglePlatform p => Color -> SceneGraph Double (Actuation p)
bgFill color = Node (Frame (translate (V3 80 72 (-2))))
                    [   Node (Term (sceneRectangle color (160, 144))) []  ]
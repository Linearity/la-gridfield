{-# LANGUAGE UndecidableInstances #-}

module Lightarrow.GridField.TerrainElement
        (   TerrainTile (..),
            TerrainCode (..),
            TerrainElement (..),
            defaultTerrainElement       ) where

import qualified Data.Map as M
import FRP.BearRiver hiding (first)
import Lightarrow

-- | Tiles for drawing terrain elements
class TerrainTile p where
    type DrawOrientation (p :: * -> *) :: *
    drawTile :: Bitmap p -> DrawOrientation p -> SceneGraph Double (Actuation p)

-- | Expressions for encoding and decoding terrain elements
class TerrainCode p where
    data TerrainElementExpression (p :: * -> *) :: *
    nullExpression :: TerrainElementExpression p

-- | An element of terrain.  A full map is made of many such elements. The
-- type families 'TerrainElementExpression' and 'DrawOrientation' are
-- indexed by the platform 'p' only to minimize the number of type variables;
-- instances of these families are likely unrelated to platform implementations.
data TerrainElement p c
    = TerrainElement {
            teCollides     :: Bool,
            teExpr         :: TerrainElementExpression p,
            teReactions    :: [c -> Event c],
            teTile         :: Maybe (Bitmap p, DrawOrientation p)
        }

defaultTerrainElement :: TerrainCode p => TerrainElement p c
defaultTerrainElement = TerrainElement {
                                teCollides = False,
                                teExpr = nullExpression,
                                teReactions = [],
                                teTile = Nothing
                            } 

instance Show (TerrainElementExpression p) => Show (TerrainElement p c) where
    show = show . teExpr
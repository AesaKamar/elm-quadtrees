module MassyQuadTree exposing (..)

import Dynamics exposing (..)
import Quadtree exposing (..)


type alias MassyQuadTree =
    { quadTree : QuadTree
    , center : MassPoint
    }

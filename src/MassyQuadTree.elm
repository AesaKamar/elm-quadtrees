module MassyQuadTree exposing (..)

import Dynamics exposing (..)
import Quadtree exposing (..)
import Pointlike exposing (..)
import Bound exposing (..)


-- import Math exposing (..)


type alias MassyQuadTree =
    { quadTree : QuadTree
    , centerOfMass : MassPoint
    }


type alias DistanceRatio =
    Float


areSufficientlyFar : DistanceRatio -> Bound -> Pointlike -> Pointlike -> Bool
areSufficientlyFar ratio bound pointA pointB =
    ratio > (distance pointA pointB) / ((boundWidth bound + boundHeight bound) / 2)

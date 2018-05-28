module MassyQuadTree exposing (..)

import Dynamics exposing (..)
import Quadtree exposing (..)
import Point exposing (..)
import Bound exposing (..)


-- import Math exposing (..)


type alias MassyQuadTree =
    { quadTree : QuadTree
    , centerOfMass : MassPoint
    }


type alias DistanceRatio =
    Float


areSufficientlyFar : DistanceRatio -> Bound -> Point -> Point -> Bool
areSufficientlyFar ratio bound pointA pointB =
    ratio > (distance pointA pointB) / ((boundWidth bound + boundHeight bound) / 2)

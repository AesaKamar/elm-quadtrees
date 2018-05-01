module Quadtree exposing (..)

import Point exposing (..)
import Bound exposing (..)


type QuadTree a
    = Internal ( QuadTree a, QuadTree a, QuadTree a, QuadTree a ) Bound
    | External (Maybe a) Bound


insert : QuadTree a -> a -> QuadTree a
insert qt new =
    case qt of
        Internal ( aQuadTree, aQuadTree2, aQuadTree3, aQuadTree4 ) bound ->
            -- TODO finish this
            Internal ( aQuadTree, aQuadTree2, aQuadTree3, aQuadTree4 ) bound

        External aMaybe bound ->
            case aMaybe of
                Nothing ->
                    -- TODO finish this
                    External (Just new) bound

                Just a ->
                    -- TODO finish this
                    External (Just a) bound

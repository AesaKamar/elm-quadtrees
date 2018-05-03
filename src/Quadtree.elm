module Quadtree exposing (..)

import Point exposing (..)
import Bound exposing (..)


type QuadTree a
    = Internal ( QuadTree a, QuadTree a, QuadTree a, QuadTree a ) Bound
    | External (Maybe a) Bound


insert : QuadTree Point -> Point -> QuadTree Point
insert qt newPt =
    case qt of
        Internal ( aQuadTree, aQuadTree2, aQuadTree3, aQuadTree4 ) bound ->
            let
                newPointPlacement =
                    (locatePoint bound newPt)
            in
                case newPointPlacement of
                    TopLeft ->
                        Internal
                            ( insert aQuadTree newPt
                            , aQuadTree2
                            , aQuadTree3
                            , aQuadTree4
                            )
                            bound

                    TopRight ->
                        Internal
                            ( aQuadTree
                            , insert aQuadTree2 newPt
                            , aQuadTree3
                            , aQuadTree4
                            )
                            bound

                    BotLeft ->
                        Internal
                            ( aQuadTree
                            , aQuadTree2
                            , insert aQuadTree3 newPt
                            , aQuadTree4
                            )
                            bound

                    BotRight ->
                        Internal
                            ( aQuadTree
                            , aQuadTree2
                            , aQuadTree3
                            , insert aQuadTree4 newPt
                            )
                            bound

        External aMaybe bound ->
            case aMaybe of
                Nothing ->
                    -- TODO finish this
                    External (Just newPt) bound

                Just existingPt ->
                    let
                        topLeftBound =
                            getQuadrantBounds TopLeft bound

                        topRightBound =
                            getQuadrantBounds TopRight bound

                        botLeftBound =
                            getQuadrantBounds BotLeft bound

                        botRightBound =
                            getQuadrantBounds BotRight bound
                    in
                        insert
                            (insert
                                (Internal
                                    ( External Nothing topLeftBound
                                    , External Nothing topRightBound
                                    , External Nothing botLeftBound
                                    , External Nothing botRightBound
                                    )
                                    bound
                                )
                                existingPt
                            )
                            newPt

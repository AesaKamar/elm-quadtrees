module Quadtree exposing (..)

import Point exposing (..)
import Bound exposing (..)


type QuadTree a
    = Internal ( QuadTree a, QuadTree a, QuadTree a, QuadTree a ) Bound
    | External (Maybe a) Bound


emptyQuadTree : QuadTree Point
emptyQuadTree =
    External Nothing emptyBound


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
                        --FIXME This is incorrect since we lose information about points that share coordinates
                        if existingPt.x == newPt.x && existingPt.y == newPt.y then
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
                        else
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

module Quadtree exposing (..)

import Pointlike exposing (..)
import Bound exposing (..)


type QuadTree a
    = Internal ( QuadTree a, QuadTree a, QuadTree a, QuadTree a ) Bound
    | External (Maybe a) Bound


emptyQuadTree : QuadTree (Pointlike a)
emptyQuadTree =
    External Nothing emptyBound


insert : QuadTree (Pointlike a) -> Pointlike a -> QuadTree (Pointlike a)
insert qt newPt =
    case qt of
        Internal ( topLeftQT, topRightQT, botLeftQT, botRightQT ) bound ->
            let
                newPointPlacement =
                    (locatePoint bound newPt)
            in
                case newPointPlacement of
                    TopLeft ->
                        Internal
                            ( insert topLeftQT newPt
                            , topRightQT
                            , botLeftQT
                            , botRightQT
                            )
                            bound

                    TopRight ->
                        Internal
                            ( topLeftQT
                            , insert topRightQT newPt
                            , botLeftQT
                            , botRightQT
                            )
                            bound

                    BotLeft ->
                        Internal
                            ( topLeftQT
                            , topRightQT
                            , insert botLeftQT newPt
                            , botRightQT
                            )
                            bound

                    BotRight ->
                        Internal
                            ( topLeftQT
                            , topRightQT
                            , botLeftQT
                            , insert botRightQT newPt
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

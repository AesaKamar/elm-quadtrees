module QuadTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Quadtree exposing (..)
import Pointlike exposing (..)
import Bound exposing (..)


pointTopLeft1 : Point
pointTopLeft1 =
    { x = 10, y = 10 }


pointTopLeft2 : Point
pointTopLeft2 =
    { x = 40, y = 40 }


pointBotRight1 : Point
pointBotRight1 =
    { x = 90, y = 90 }


outerBound : Bound
outerBound =
    { topLeftmost = { x = 0, y = 0 }
    , botRightmost = { x = 100, y = 100 }
    }


noPoint : Maybe Point
noPoint =
    Nothing


suite : Test
suite =
    describe "Quadtree insertion"
        [ test "into empty leaf "
            (\_ ->
                let
                    fixture : QuadTree Point
                    fixture =
                        External noPoint outerBound

                    actual =
                        insert fixture pointTopLeft1

                    expected =
                        External (Just pointTopLeft1) outerBound
                in
                    Expect.equal actual expected
            )
        , test "insert into tree with 4 empty quads at TopLeft"
            (\_ ->
                let
                    fixture : QuadTree Point
                    fixture =
                        Internal
                            ( External noPoint (getQuadrantBounds TopLeft outerBound)
                            , External noPoint (getQuadrantBounds TopRight outerBound)
                            , External noPoint (getQuadrantBounds BotLeft outerBound)
                            , External noPoint (getQuadrantBounds BotRight outerBound)
                            )
                            outerBound

                    actual =
                        insert fixture pointTopLeft1

                    expected =
                        Internal
                            ( External (Just pointTopLeft1) (getQuadrantBounds TopLeft outerBound)
                            , External noPoint (getQuadrantBounds TopRight outerBound)
                            , External noPoint (getQuadrantBounds BotLeft outerBound)
                            , External noPoint (getQuadrantBounds BotRight outerBound)
                            )
                            outerBound
                in
                    Expect.equal actual expected
            )
        , test "insert into tree with 4 empty quads at botRightBound"
            (\_ ->
                let
                    fixture : QuadTree Point
                    fixture =
                        Internal
                            ( External noPoint (getQuadrantBounds TopLeft outerBound)
                            , External noPoint (getQuadrantBounds TopRight outerBound)
                            , External noPoint (getQuadrantBounds BotLeft outerBound)
                            , External noPoint (getQuadrantBounds BotRight outerBound)
                            )
                            outerBound

                    actual =
                        insert fixture pointBotRight1

                    expected =
                        Internal
                            ( External noPoint (getQuadrantBounds TopLeft outerBound)
                            , External noPoint (getQuadrantBounds TopRight outerBound)
                            , External noPoint (getQuadrantBounds BotLeft outerBound)
                            , External (Just pointBotRight1) (getQuadrantBounds BotRight outerBound)
                            )
                            outerBound
                in
                    Expect.equal actual expected
            )
        , test "insert into tree with conflicting occupants in the topLeft"
            (\_ ->
                let
                    fixture : QuadTree Point
                    fixture =
                        Internal
                            ( External noPoint (getQuadrantBounds TopLeft outerBound)
                            , External noPoint (getQuadrantBounds TopRight outerBound)
                            , External noPoint (getQuadrantBounds BotLeft outerBound)
                            , External noPoint (getQuadrantBounds BotRight outerBound)
                            )
                            outerBound

                    actual =
                        insert (insert fixture pointTopLeft1) pointTopLeft2

                    expected =
                        Internal
                            ( let
                                topLeftBound =
                                    (getQuadrantBounds TopLeft outerBound)
                              in
                                Internal
                                    ( External (Just pointTopLeft1) (getQuadrantBounds TopLeft topLeftBound)
                                    , External noPoint (getQuadrantBounds TopRight topLeftBound)
                                    , External noPoint (getQuadrantBounds BotLeft topLeftBound)
                                    , External (Just pointTopLeft2) (getQuadrantBounds BotRight topLeftBound)
                                    )
                                    topLeftBound
                            , External noPoint (getQuadrantBounds TopRight outerBound)
                            , External noPoint (getQuadrantBounds BotLeft outerBound)
                            , External noPoint (getQuadrantBounds BotRight outerBound)
                            )
                            outerBound
                in
                    Expect.equal actual expected
            )
        ]

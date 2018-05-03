module QuadTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Quadtree exposing (..)
import Point exposing (..)
import Bound exposing (..)


randomPoint : Point
randomPoint =
    { x = 10, y = 10 }


outerBound : Bound
outerBound =
    { topLeftmost = { x = 0, y = 0 }
    , botRightmost = { x = 100, y = 100 }
    }


nothingPoint : Maybe Point
nothingPoint =
    Nothing


suite : Test
suite =
    describe "Quadtree insertion"
        [ test "into empty leaf "
            (\_ ->
                let
                    fixture : QuadTree Point
                    fixture =
                        External nothingPoint outerBound

                    actual =
                        insert fixture randomPoint

                    expected =
                        External (Just randomPoint) outerBound
                in
                    Expect.equal actual expected
            )
        , test "insert into tree with 4 empty quads at TopLeft"
            (\_ ->
                let
                    fixture : QuadTree Point
                    fixture =
                        Internal
                            ( External nothingPoint (getQuadrantBounds TopLeft outerBound)
                            , External nothingPoint (getQuadrantBounds TopRight outerBound)
                            , External nothingPoint (getQuadrantBounds BotLeft outerBound)
                            , External nothingPoint (getQuadrantBounds BotRight outerBound)
                            )
                            outerBound

                    actual =
                        insert fixture randomPoint

                    expected =
                        Internal
                            ( External (Just randomPoint) (getQuadrantBounds TopLeft outerBound)
                            , External nothingPoint (getQuadrantBounds TopRight outerBound)
                            , External nothingPoint (getQuadrantBounds BotLeft outerBound)
                            , External nothingPoint (getQuadrantBounds BotRight outerBound)
                            )
                            outerBound
                in
                    Expect.equal actual expected
            )
        ]

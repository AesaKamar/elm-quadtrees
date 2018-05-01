module BoundTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Bound exposing (..)
import Point exposing (..)


box : Bound
box =
    { topLeftmost = { x = 0, y = 0 }, botRightmost = { x = 100, y = 100 } }


suite : Test
suite =
    describe "Bound"
        [ test "center" <|
            \_ ->
                Expect.equal (center box) ({ x = 50, y = 50 })
        , describe "locatePoint"
            [ test "TopLeft" <|
                \_ ->
                    let
                        location =
                            locatePoint box { x = 25, y = 25 }
                    in
                        Expect.equal location TopLeft
            , test "TopRight" <|
                \_ ->
                    let
                        location =
                            locatePoint box { x = 75, y = 25 }
                    in
                        Expect.equal location TopRight
            , test "BotLeft" <|
                \_ ->
                    let
                        location =
                            locatePoint box { x = 25, y = 75 }
                    in
                        Expect.equal location BotLeft
            , test "BotRight" <|
                \_ ->
                    let
                        location =
                            locatePoint box { x = 75, y = 75 }
                    in
                        Expect.equal location BotRight
            ]
        ]

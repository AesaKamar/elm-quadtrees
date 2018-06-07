module PointTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Pointlike exposing (..)


suite : Test
suite =
    describe "Point"
        [ describe "between"
            [ test "out of range to the left" <|
                \_ ->
                    let
                        left =
                            5

                        right =
                            10
                    in
                        Expect.false "String" (between left right 5)
            , test "out of range to the right" <|
                \_ ->
                    let
                        left =
                            5

                        right =
                            10
                    in
                        Expect.false "String" (between left right 20)
            , test "within range" <|
                \_ ->
                    let
                        left =
                            5

                        right =
                            10
                    in
                        Expect.true "String" (between left right 7)
            ]
        ]

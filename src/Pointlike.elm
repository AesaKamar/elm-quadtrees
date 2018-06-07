module Pointlike exposing (..)

-- TODO Use extensible records as an interface


type alias Pointlike a =
    { a | x : Float, y : Float }


type alias Point =
    Pointlike {}


between : Float -> Float -> Float -> Bool
between start end test =
    test < end && test > start


distance : Pointlike a -> Pointlike a -> Float
distance pointA pointB =
    sqrt ((pointB.x - pointA.x) ^ 2) + ((pointB.y - pointA.y) ^ 2)

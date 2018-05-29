module Point exposing (..)

-- TODO Use extensible records as an interface


type alias Point p =
    { p | x : Float, y : Float }


between : Float -> Float -> Float -> Bool
between start end test =
    test < end && test > start


distance : Point a -> Point a -> Float
distance pointA pointB =
    sqrt ((pointB.x - pointA.x) ^ 2) + ((pointB.y - pointA.y) ^ 2)

module Point exposing (..)

-- TODO Use extensible records as an interface


type alias Point =
    { x : Float, y : Float }


between : Float -> Float -> Float -> Bool
between start end test =
    test < end && test > start

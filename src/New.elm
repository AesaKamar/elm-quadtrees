module Example exposing (..)


type alias ExtensiblePoint p =
    { p
        | x : Float
        , y : Float
    }


type alias ExtensibleRectangle r p =
    { r
        | topLeft : ExtensiblePoint p
        , topRight : ExtensiblePoint p
    }


isInside : ExtensiblePoint p -> ExtensibleRectangle r p -> Bool
isInside pExtensiblePoint rExtensibleRectangle =
    False

module Bound exposing (..)

import Point exposing (..)


type RelativePostion
    = TopLeft
    | TopRight
    | BotLeft
    | BotRight


type alias Bound =
    { topLeftmost : Point, botRightmost : Point }


center : Bound -> Point
center bound =
    { x = (bound.botRightmost.x - bound.topLeftmost.x) / 2 + bound.topLeftmost.x
    , y = (bound.botRightmost.y - bound.topLeftmost.y) / 2 + bound.topLeftmost.y
    }


inside : Point -> Point -> Point -> Bool
inside topLeft botRight test =
    (between topLeft.x botRight.x test.x) && (between topLeft.y botRight.y test.y)


locatePoint : Bound -> Point -> RelativePostion
locatePoint bound test =
    let
        ctr =
            center bound
    in
        if inside bound.topLeftmost ctr test then
            TopLeft
        else if inside ctr bound.botRightmost test then
            BotRight
        else if
            inside
                { x = ctr.x, y = bound.topLeftmost.y }
                { x = bound.botRightmost.x, y = ctr.y }
                test
        then
            TopRight
        else
            BotLeft


getQuadrantBounds : RelativePostion -> Bound -> Bound
getQuadrantBounds relativePostion outerBound =
    case relativePostion of
        TopLeft ->
            { topLeftmost = outerBound.topLeftmost
            , botRightmost = center outerBound
            }

        TopRight ->
            { topLeftmost = { x = (center outerBound).x, y = outerBound.topLeftmost.y }
            , botRightmost = { x = outerBound.botRightmost.x, y = (center outerBound).y }
            }

        BotLeft ->
            { topLeftmost = { x = outerBound.topLeftmost.x, y = (center outerBound).y }
            , botRightmost = { x = (center outerBound).x, y = outerBound.botRightmost.y }
            }

        BotRight ->
            { topLeftmost = (center outerBound)
            , botRightmost = outerBound.botRightmost
            }

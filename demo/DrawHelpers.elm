module DrawHelpers exposing (..)

import Quadtree exposing (..)
import Pointlike exposing (..)
import Svg
import Svg.Attributes as SvgA


drawQuadTree : QuadTree Pointlike -> List (Svg.Svg msg)
drawQuadTree qt =
    case qt of
        Internal ( aQuadTree, aQuadTree2, aQuadTree3, aQuadTree4 ) bound ->
            (drawQuadTree aQuadTree)
                ++ (drawQuadTree aQuadTree2)
                ++ (drawQuadTree aQuadTree3)
                ++ (drawQuadTree aQuadTree4)

        External aMaybe bound ->
            case aMaybe of
                Just pt ->
                    [ drawBound bound
                    , drawPoint pt
                    ]

                Nothing ->
                    [ drawBound bound ]


drawPoint pt =
    Svg.circle
        [ SvgA.cx (pt.x |> toString)
        , SvgA.cy (pt.y |> toString)
        , SvgA.r ("3")
        , SvgA.fill "red"
        ]
        []


drawBound bound =
    Svg.rect
        [ SvgA.x (bound.topLeftmost.x |> toString)
        , SvgA.y (bound.topLeftmost.y |> toString)
        , SvgA.width ((bound.botRightmost.x - bound.topLeftmost.x) |> toString)
        , SvgA.height ((bound.botRightmost.y - bound.topLeftmost.y) |> toString)
        , SvgA.style "outline-style:solid; outline-offset:-1px; outline-width:2px; outline-color:black"
        , SvgA.fill "none"
        ]
        []

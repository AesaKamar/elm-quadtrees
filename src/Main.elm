module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA
import Window exposing (..)
import Quadtree exposing (..)
import Point exposing (..)
import Task exposing (perform)
import Bound exposing (..)
import Svg
import Svg.Attributes as SvgA


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { quadTree : QuadTree Point
    , points : List Point
    , windowSize : Bound
    }


initialModel : Model
initialModel =
    { quadTree = emptyQuadTree
    , points = [ { x = 40, y = 40 } ]
    , windowSize =
        { topLeftmost = { x = 0, y = 0 }
        , botRightmost = { x = 100, y = 100 }
        }
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Task.perform WindowResize Window.size
    )



-- UPDATE


type Msg
    = WindowResize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize windowSizing ->
            let
                bound =
                    { topLeftmost = { x = 0, y = 0 }
                    , botRightmost =
                        { x = windowSizing.width |> toFloat
                        , y = windowSizing.height |> toFloat
                        }
                    }
            in
                ( { quadTree = emptyQuadTree
                  , points = model.points
                  , windowSize = bound
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowResize



-- VIEW


viewQuadTree : QuadTree Point -> List (Svg.Svg Msg)
viewQuadTree qt =
    case qt of
        Internal ( aQuadTree, aQuadTree2, aQuadTree3, aQuadTree4 ) bound ->
            (viewQuadTree aQuadTree)
                ++ (viewQuadTree aQuadTree2)
                ++ (viewQuadTree aQuadTree3)
                ++ (viewQuadTree aQuadTree4)

        External aMaybe bound ->
            case aMaybe of
                Just pt ->
                    [ Svg.rect
                        [ SvgA.x (bound.topLeftmost.x |> toString)
                        , SvgA.y (bound.topLeftmost.y |> toString)
                        , SvgA.fill "none"
                        , SvgA.stroke "black"
                        ]
                        []
                    , Svg.circle
                        [ SvgA.cx (pt.x |> toString)
                        , SvgA.cy (pt.y |> toString)
                        , SvgA.fill "black"
                        ]
                        []
                    ]

                Nothing ->
                    [ Svg.rect
                        [ SvgA.x (bound.topLeftmost.x |> toString)
                        , SvgA.y (bound.topLeftmost.y |> toString)
                        , SvgA.fill "none"
                        , SvgA.stroke "pink"
                        ]
                        []
                    ]


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg
            [ SvgA.height (model.windowSize.botRightmost.y |> toString)
            , SvgA.width (model.windowSize.botRightmost.x |> toString)
            ]
            (viewQuadTree model.quadTree)
        ]
